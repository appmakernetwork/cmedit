{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples #-}
-- | JPEG XL VarDCT building blocks (ISO/IEC 18181-1 Annex I and J): the
-- transforms, dequantisation matrices, coefficient orders and the
-- restoration filters. The orchestration lives in "Cmedit.Jxl".
--
-- Conventions follow jxl-oxide (and so libjxl): a varblock's coefficients
-- sit in the frame's float planes at the varblock's own pixel footprint, x
-- being horizontal frequency; the 1-D transforms are the scaled pair
-- @X_k = (1/N) Σ x_n cos(..) · (√2 for k > 0)@ and its exact inverse, so a
-- DC coefficient equal to the LF value reproduces a flat block.
module Cmedit.JxlVarDct
  ( FPlane
    -- * Transform types
  , dctW8, dctH8, dctParam, dctOrder, dctTranspose, numDctSelect
  , orderBW, orderBH
    -- * Transforms
  , Scratch, newScratch, idctBlock, llfFromLf
    -- * Dequantisation matrices
  , DqParams, defaultDqParams, readDqParams, buildMatrix, matrixW, matrixH
    -- * Coefficient orders
  , naturalOrder
    -- * Filters
  , adaptiveLfSmoothing, gaborish, epf, filterBands
    -- * HF coefficients
  , HfCtx(..), decodeHfGroup
  ) where

import Control.Monad.ST.Unsafe (unsafeIOToST, unsafeSTToIO)
import Cmedit.JxlPar (parOrdered)

import Control.Monad (forM, forM_, replicateM, unless, when)
import GHC.ST (ST(..))
import GHC.Exts (Int(I#), copyMutableByteArray#)
import Data.Array.Base (STUArray(STUArray), unsafeAt, unsafeRead, unsafeWrite)
import Data.Array.ST (STUArray, newArray)
import Data.Array.Unboxed (UArray, listArray)
import qualified Data.Array.Unboxed as U
import Data.Bits

import Data.Array (Array)
import qualified Data.Array as A
import Data.Int (Int32)

import Cmedit.JxlBits
import Cmedit.JxlEntropy

type FPlane s = STUArray s Int Float

------------------------------------------------------------------------------
-- Transform types (DctSelect), in the codestream's numbering

numDctSelect :: Int
numDctSelect = 27

dctW8, dctH8, dctParam, dctOrder :: UArray Int Int
dctW8 = listArray (0, 26) [1,1,1,1,2,4,1,2,1,4,2,4,1,1,1,1,1,1,8,4,8,16,8,16,32,16,32]
dctH8 = listArray (0, 26) [1,1,1,1,2,4,2,1,4,1,4,2,1,1,1,1,1,1,8,8,4,16,16,8,32,32,16]
dctParam = listArray (0, 26) [0,1,2,3,4,5,6,6,7,7,8,8,9,9,10,10,10,10,11,12,12,13,14,14,15,16,16]
dctOrder = listArray (0, 26) [0,1,1,1,2,3,4,4,5,5,6,6,1,1,1,1,1,1,7,8,8,9,10,10,11,12,12]

-- | Coefficients are coded in the wide orientation; a tall (or square)
-- DCT stores them transposed. The 8x8 special transforms never do.
dctTranspose :: Int -> Bool
dctTranspose t
  | t `elem` [1, 2, 3, 12, 13, 14, 15, 16, 17] = False
  | otherwise = dctH8 `unsafeAt` t >= dctW8 `unsafeAt` t

orderBW, orderBH :: UArray Int Int
orderBW = listArray (0, 12) [8,8,16,32,16,32,32,64,64,128,128,256,256]
orderBH = listArray (0, 12) [8,8,16,32,8,8,16,64,32,128,64,256,128]

-- Matrix dimensions per parameter set (wide orientation).
matrixW, matrixH :: UArray Int Int
matrixW = listArray (0, 16) [8,8,8,8,16,32,16,32,32,8,8,64,64,128,128,256,256]
matrixH = listArray (0, 16) [8,8,8,8,16,32,8,8,16,8,8,64,32,128,64,256,128]

------------------------------------------------------------------------------
-- 1-D transforms (radix-2, after Perera & Liu as libjxl and jxl-oxide do)

-- [n/2 + k] = 1 / (2 cos((2k + 1) pi / 2n)), for n = 4 .. 256.
secTab :: UArray Int Float
secTab = listArray (0, 255)
  ([0, 0] ++ concat [ [ realToFrac (1 / (2 * cos ((2 * fromIntegral k + 1) * pi / (2 * fromIntegral n))) :: Double)
                      | k <- [0 .. n `div` 2 - 1] ]
                    | n <- [4, 8, 16, 32, 64, 128, 256 :: Int] ])

sqrt2 :: Float
sqrt2 = 1.4142135623730951

-- | LLF / small-transform scratch (at most 32x32), two disjoint 256-value
-- recursive work regions, and sixteen gathered columns of at most 256 values.
data Scratch s = Scratch !(FPlane s) !(FPlane s) !(FPlane s)

newScratch :: ST s (Scratch s)
newScratch = Scratch <$> newArray (0, 1023) 0 <*> newArray (0, 511) 0 <*> newArray (0, 16 * 256 - 1) 0

{-# INLINE rd #-}
rd :: FPlane s -> Int -> ST s Float
rd = unsafeRead

{-# INLINE wr #-}
wr :: FPlane s -> Int -> Float -> ST s ()
wr = unsafeWrite

-- Inverse 1-D transform of a[off .. off+n), scratch t[toff .. toff+n).
idct1 :: FPlane s -> Int -> Int -> FPlane s -> Int -> ST s ()
idct1 a off n t toff
  | n <= 1 = pure ()
  | n == 2 = do
      x0 <- rd a off; x1 <- rd a (off + 1)
      wr a off (x0 + x1); wr a (off + 1) (x0 - x1)
  | n == 4 = do
      i0 <- rd a off; i1 <- rd a (off + 1); i2 <- rd a (off + 2); i3 <- rd a (off + 3)
      let tmp0 = i1 * sqrt2
          tmp1 = i1 + i3
          o0 = (tmp0 + tmp1) * 0.5411961
          o1 = (tmp0 - tmp1) * 1.306563
          s02 = i0 + i2
          d02 = i0 - i2
      wr a off (s02 + o0); wr a (off + 1) (d02 + o1)
      wr a (off + 2) (d02 - o1); wr a (off + 3) (s02 - o0)
  | n == 8 = do
      i0 <- rd a off; i1 <- rd a (off + 1); i2 <- rd a (off + 2); i3 <- rd a (off + 3)
      i4 <- rd a (off + 4); i5 <- rd a (off + 5); i6 <- rd a (off + 6); i7 <- rd a (off + 7)
      let -- even half: inverse DCT-4 of (i0, i2, i4, i6)
          et0 = i2 * sqrt2
          et1 = i2 + i6
          eo0 = (et0 + et1) * 0.5411961
          eo1 = (et0 - et1) * 1.306563
          es02 = i0 + i4
          ed02 = i0 - i4
          e0 = es02 + eo0; e1 = ed02 + eo1; e2 = ed02 - eo1; e3 = es02 - eo0
          -- odd half: inverse DCT-4 of (i1 √2, i3 + i1, i5 + i3, i7 + i5)
          q0 = i1 * sqrt2; q1 = i3 + i1; q2 = i5 + i3; q3 = i7 + i5
          ot0 = q1 * sqrt2
          ot1 = q1 + q3
          oo0 = (ot0 + ot1) * 0.5411961
          oo1 = (ot0 - ot1) * 1.306563
          os02 = q0 + q2
          od02 = q0 - q2
          r0 = (os02 + oo0) * 0.5097955791041592
          r1 = (od02 + oo1) * 0.6013448869350453
          r2 = (od02 - oo1) * 0.8999762231364156
          r3 = (os02 - oo0) * 2.5629154477415055
      wr a off (e0 + r0); wr a (off + 7) (e0 - r0)
      wr a (off + 1) (e1 + r1); wr a (off + 6) (e1 - r1)
      wr a (off + 2) (e2 + r2); wr a (off + 5) (e2 - r2)
      wr a (off + 3) (e3 + r3); wr a (off + 4) (e3 - r3)
  -- Expanded radix-2 graphs retain the existing Float operation order
  -- while avoiding recursive scratch-array traffic at these two sizes.
  | n == 16 = do
      i0 <- rd a (off + 0)
      i1 <- rd a (off + 1)
      i2 <- rd a (off + 2)
      i3 <- rd a (off + 3)
      i4 <- rd a (off + 4)
      i5 <- rd a (off + 5)
      i6 <- rd a (off + 6)
      i7 <- rd a (off + 7)
      i8 <- rd a (off + 8)
      i9 <- rd a (off + 9)
      i10 <- rd a (off + 10)
      i11 <- rd a (off + 11)
      i12 <- rd a (off + 12)
      i13 <- rd a (off + 13)
      i14 <- rd a (off + 14)
      i15 <- rd a (off + 15)
      let v0 = i4 * sqrt2
          v1 = i4 + i12
          v2 = (v0 + v1) * 0.5411961
          v3 = (v0 - v1) * 1.306563
          v4 = i0 + i8
          v5 = i0 - i8
          v6 = v4 + v2
          v7 = v5 + v3
          v8 = v5 - v3
          v9 = v4 - v2
          v10 = i2 * sqrt2
          v11 = i6 + i2
          v12 = i10 + i6
          v13 = i14 + i10
          v14 = v11 * sqrt2
          v15 = v11 + v13
          v16 = (v14 + v15) * 0.5411961
          v17 = (v14 - v15) * 1.306563
          v18 = v10 + v12
          v19 = v10 - v12
          v20 = v18 + v16
          v21 = v19 + v17
          v22 = v19 - v17
          v23 = v18 - v16
          v24 = v20 * 0.5097955791041592
          v25 = v21 * 0.6013448869350453
          v26 = v22 * 0.8999762231364156
          v27 = v23 * 2.5629154477415055
          v28 = v6 + v24
          v29 = v7 + v25
          v30 = v8 + v26
          v31 = v9 + v27
          v32 = v9 - v27
          v33 = v8 - v26
          v34 = v7 - v25
          v35 = v6 - v24
          v36 = i1 * sqrt2
          v37 = i3 + i1
          v38 = i5 + i3
          v39 = i7 + i5
          v40 = i9 + i7
          v41 = i11 + i9
          v42 = i13 + i11
          v43 = i15 + i13
          v44 = v38 * sqrt2
          v45 = v38 + v42
          v46 = (v44 + v45) * 0.5411961
          v47 = (v44 - v45) * 1.306563
          v48 = v36 + v40
          v49 = v36 - v40
          v50 = v48 + v46
          v51 = v49 + v47
          v52 = v49 - v47
          v53 = v48 - v46
          v54 = v37 * sqrt2
          v55 = v39 + v37
          v56 = v41 + v39
          v57 = v43 + v41
          v58 = v55 * sqrt2
          v59 = v55 + v57
          v60 = (v58 + v59) * 0.5411961
          v61 = (v58 - v59) * 1.306563
          v62 = v54 + v56
          v63 = v54 - v56
          v64 = v62 + v60
          v65 = v63 + v61
          v66 = v63 - v61
          v67 = v62 - v60
          v68 = v64 * 0.5097955791041592
          v69 = v65 * 0.6013448869350453
          v70 = v66 * 0.8999762231364156
          v71 = v67 * 2.5629154477415055
          v72 = v50 + v68
          v73 = v51 + v69
          v74 = v52 + v70
          v75 = v53 + v71
          v76 = v53 - v71
          v77 = v52 - v70
          v78 = v51 - v69
          v79 = v50 - v68
          v80 = v72 * 0.5024192861881557
          v81 = v73 * 0.5224986149396889
          v82 = v74 * 0.5669440348163577
          v83 = v75 * 0.6468217833599901
          v84 = v76 * 0.7881546234512502
          v85 = v77 * 1.060677685990347
          v86 = v78 * 1.7224470982383342
          v87 = v79 * 5.101148618689155
          v88 = v28 + v80
          v89 = v29 + v81
          v90 = v30 + v82
          v91 = v31 + v83
          v92 = v32 + v84
          v93 = v33 + v85
          v94 = v34 + v86
          v95 = v35 + v87
          v96 = v35 - v87
          v97 = v34 - v86
          v98 = v33 - v85
          v99 = v32 - v84
          v100 = v31 - v83
          v101 = v30 - v82
          v102 = v29 - v81
          v103 = v28 - v80
      wr a (off + 0) v88
      wr a (off + 1) v89
      wr a (off + 2) v90
      wr a (off + 3) v91
      wr a (off + 4) v92
      wr a (off + 5) v93
      wr a (off + 6) v94
      wr a (off + 7) v95
      wr a (off + 8) v96
      wr a (off + 9) v97
      wr a (off + 10) v98
      wr a (off + 11) v99
      wr a (off + 12) v100
      wr a (off + 13) v101
      wr a (off + 14) v102
      wr a (off + 15) v103
  | n == 32 = do
      i0 <- rd a (off + 0)
      i1 <- rd a (off + 1)
      i2 <- rd a (off + 2)
      i3 <- rd a (off + 3)
      i4 <- rd a (off + 4)
      i5 <- rd a (off + 5)
      i6 <- rd a (off + 6)
      i7 <- rd a (off + 7)
      i8 <- rd a (off + 8)
      i9 <- rd a (off + 9)
      i10 <- rd a (off + 10)
      i11 <- rd a (off + 11)
      i12 <- rd a (off + 12)
      i13 <- rd a (off + 13)
      i14 <- rd a (off + 14)
      i15 <- rd a (off + 15)
      i16 <- rd a (off + 16)
      i17 <- rd a (off + 17)
      i18 <- rd a (off + 18)
      i19 <- rd a (off + 19)
      i20 <- rd a (off + 20)
      i21 <- rd a (off + 21)
      i22 <- rd a (off + 22)
      i23 <- rd a (off + 23)
      i24 <- rd a (off + 24)
      i25 <- rd a (off + 25)
      i26 <- rd a (off + 26)
      i27 <- rd a (off + 27)
      i28 <- rd a (off + 28)
      i29 <- rd a (off + 29)
      i30 <- rd a (off + 30)
      i31 <- rd a (off + 31)
      let v0 = i8 * sqrt2
          v1 = i8 + i24
          v2 = (v0 + v1) * 0.5411961
          v3 = (v0 - v1) * 1.306563
          v4 = i0 + i16
          v5 = i0 - i16
          v6 = v4 + v2
          v7 = v5 + v3
          v8 = v5 - v3
          v9 = v4 - v2
          v10 = i4 * sqrt2
          v11 = i12 + i4
          v12 = i20 + i12
          v13 = i28 + i20
          v14 = v11 * sqrt2
          v15 = v11 + v13
          v16 = (v14 + v15) * 0.5411961
          v17 = (v14 - v15) * 1.306563
          v18 = v10 + v12
          v19 = v10 - v12
          v20 = v18 + v16
          v21 = v19 + v17
          v22 = v19 - v17
          v23 = v18 - v16
          v24 = v20 * 0.5097955791041592
          v25 = v21 * 0.6013448869350453
          v26 = v22 * 0.8999762231364156
          v27 = v23 * 2.5629154477415055
          v28 = v6 + v24
          v29 = v7 + v25
          v30 = v8 + v26
          v31 = v9 + v27
          v32 = v9 - v27
          v33 = v8 - v26
          v34 = v7 - v25
          v35 = v6 - v24
          v36 = i2 * sqrt2
          v37 = i6 + i2
          v38 = i10 + i6
          v39 = i14 + i10
          v40 = i18 + i14
          v41 = i22 + i18
          v42 = i26 + i22
          v43 = i30 + i26
          v44 = v38 * sqrt2
          v45 = v38 + v42
          v46 = (v44 + v45) * 0.5411961
          v47 = (v44 - v45) * 1.306563
          v48 = v36 + v40
          v49 = v36 - v40
          v50 = v48 + v46
          v51 = v49 + v47
          v52 = v49 - v47
          v53 = v48 - v46
          v54 = v37 * sqrt2
          v55 = v39 + v37
          v56 = v41 + v39
          v57 = v43 + v41
          v58 = v55 * sqrt2
          v59 = v55 + v57
          v60 = (v58 + v59) * 0.5411961
          v61 = (v58 - v59) * 1.306563
          v62 = v54 + v56
          v63 = v54 - v56
          v64 = v62 + v60
          v65 = v63 + v61
          v66 = v63 - v61
          v67 = v62 - v60
          v68 = v64 * 0.5097955791041592
          v69 = v65 * 0.6013448869350453
          v70 = v66 * 0.8999762231364156
          v71 = v67 * 2.5629154477415055
          v72 = v50 + v68
          v73 = v51 + v69
          v74 = v52 + v70
          v75 = v53 + v71
          v76 = v53 - v71
          v77 = v52 - v70
          v78 = v51 - v69
          v79 = v50 - v68
          v80 = v72 * 0.5024192861881557
          v81 = v73 * 0.5224986149396889
          v82 = v74 * 0.5669440348163577
          v83 = v75 * 0.6468217833599901
          v84 = v76 * 0.7881546234512502
          v85 = v77 * 1.060677685990347
          v86 = v78 * 1.7224470982383342
          v87 = v79 * 5.101148618689155
          v88 = v28 + v80
          v89 = v29 + v81
          v90 = v30 + v82
          v91 = v31 + v83
          v92 = v32 + v84
          v93 = v33 + v85
          v94 = v34 + v86
          v95 = v35 + v87
          v96 = v35 - v87
          v97 = v34 - v86
          v98 = v33 - v85
          v99 = v32 - v84
          v100 = v31 - v83
          v101 = v30 - v82
          v102 = v29 - v81
          v103 = v28 - v80
          v104 = i1 * sqrt2
          v105 = i3 + i1
          v106 = i5 + i3
          v107 = i7 + i5
          v108 = i9 + i7
          v109 = i11 + i9
          v110 = i13 + i11
          v111 = i15 + i13
          v112 = i17 + i15
          v113 = i19 + i17
          v114 = i21 + i19
          v115 = i23 + i21
          v116 = i25 + i23
          v117 = i27 + i25
          v118 = i29 + i27
          v119 = i31 + i29
          v120 = v108 * sqrt2
          v121 = v108 + v116
          v122 = (v120 + v121) * 0.5411961
          v123 = (v120 - v121) * 1.306563
          v124 = v104 + v112
          v125 = v104 - v112
          v126 = v124 + v122
          v127 = v125 + v123
          v128 = v125 - v123
          v129 = v124 - v122
          v130 = v106 * sqrt2
          v131 = v110 + v106
          v132 = v114 + v110
          v133 = v118 + v114
          v134 = v131 * sqrt2
          v135 = v131 + v133
          v136 = (v134 + v135) * 0.5411961
          v137 = (v134 - v135) * 1.306563
          v138 = v130 + v132
          v139 = v130 - v132
          v140 = v138 + v136
          v141 = v139 + v137
          v142 = v139 - v137
          v143 = v138 - v136
          v144 = v140 * 0.5097955791041592
          v145 = v141 * 0.6013448869350453
          v146 = v142 * 0.8999762231364156
          v147 = v143 * 2.5629154477415055
          v148 = v126 + v144
          v149 = v127 + v145
          v150 = v128 + v146
          v151 = v129 + v147
          v152 = v129 - v147
          v153 = v128 - v146
          v154 = v127 - v145
          v155 = v126 - v144
          v156 = v105 * sqrt2
          v157 = v107 + v105
          v158 = v109 + v107
          v159 = v111 + v109
          v160 = v113 + v111
          v161 = v115 + v113
          v162 = v117 + v115
          v163 = v119 + v117
          v164 = v158 * sqrt2
          v165 = v158 + v162
          v166 = (v164 + v165) * 0.5411961
          v167 = (v164 - v165) * 1.306563
          v168 = v156 + v160
          v169 = v156 - v160
          v170 = v168 + v166
          v171 = v169 + v167
          v172 = v169 - v167
          v173 = v168 - v166
          v174 = v157 * sqrt2
          v175 = v159 + v157
          v176 = v161 + v159
          v177 = v163 + v161
          v178 = v175 * sqrt2
          v179 = v175 + v177
          v180 = (v178 + v179) * 0.5411961
          v181 = (v178 - v179) * 1.306563
          v182 = v174 + v176
          v183 = v174 - v176
          v184 = v182 + v180
          v185 = v183 + v181
          v186 = v183 - v181
          v187 = v182 - v180
          v188 = v184 * 0.5097955791041592
          v189 = v185 * 0.6013448869350453
          v190 = v186 * 0.8999762231364156
          v191 = v187 * 2.5629154477415055
          v192 = v170 + v188
          v193 = v171 + v189
          v194 = v172 + v190
          v195 = v173 + v191
          v196 = v173 - v191
          v197 = v172 - v190
          v198 = v171 - v189
          v199 = v170 - v188
          v200 = v192 * 0.5024192861881557
          v201 = v193 * 0.5224986149396889
          v202 = v194 * 0.5669440348163577
          v203 = v195 * 0.6468217833599901
          v204 = v196 * 0.7881546234512502
          v205 = v197 * 1.060677685990347
          v206 = v198 * 1.7224470982383342
          v207 = v199 * 5.101148618689155
          v208 = v148 + v200
          v209 = v149 + v201
          v210 = v150 + v202
          v211 = v151 + v203
          v212 = v152 + v204
          v213 = v153 + v205
          v214 = v154 + v206
          v215 = v155 + v207
          v216 = v155 - v207
          v217 = v154 - v206
          v218 = v153 - v205
          v219 = v152 - v204
          v220 = v151 - v203
          v221 = v150 - v202
          v222 = v149 - v201
          v223 = v148 - v200
          v224 = v208 * 0.5006029982351963
          v225 = v209 * 0.5054709598975436
          v226 = v210 * 0.5154473099226246
          v227 = v211 * 0.5310425910897841
          v228 = v212 * 0.5531038960344445
          v229 = v213 * 0.5829349682061339
          v230 = v214 * 0.6225041230356648
          v231 = v215 * 0.6748083414550057
          v232 = v216 * 0.7445362710022986
          v233 = v217 * 0.8393496454155268
          v234 = v218 * 0.9725682378619608
          v235 = v219 * 1.1694399334328847
          v236 = v220 * 1.4841646163141662
          v237 = v221 * 2.057781009953411
          v238 = v222 * 3.407608418468719
          v239 = v223 * 10.190008123548033
          v240 = v88 + v224
          v241 = v89 + v225
          v242 = v90 + v226
          v243 = v91 + v227
          v244 = v92 + v228
          v245 = v93 + v229
          v246 = v94 + v230
          v247 = v95 + v231
          v248 = v96 + v232
          v249 = v97 + v233
          v250 = v98 + v234
          v251 = v99 + v235
          v252 = v100 + v236
          v253 = v101 + v237
          v254 = v102 + v238
          v255 = v103 + v239
          v256 = v103 - v239
          v257 = v102 - v238
          v258 = v101 - v237
          v259 = v100 - v236
          v260 = v99 - v235
          v261 = v98 - v234
          v262 = v97 - v233
          v263 = v96 - v232
          v264 = v95 - v231
          v265 = v94 - v230
          v266 = v93 - v229
          v267 = v92 - v228
          v268 = v91 - v227
          v269 = v90 - v226
          v270 = v89 - v225
          v271 = v88 - v224
      wr a (off + 0) v240
      wr a (off + 1) v241
      wr a (off + 2) v242
      wr a (off + 3) v243
      wr a (off + 4) v244
      wr a (off + 5) v245
      wr a (off + 6) v246
      wr a (off + 7) v247
      wr a (off + 8) v248
      wr a (off + 9) v249
      wr a (off + 10) v250
      wr a (off + 11) v251
      wr a (off + 12) v252
      wr a (off + 13) v253
      wr a (off + 14) v254
      wr a (off + 15) v255
      wr a (off + 16) v256
      wr a (off + 17) v257
      wr a (off + 18) v258
      wr a (off + 19) v259
      wr a (off + 20) v260
      wr a (off + 21) v261
      wr a (off + 22) v262
      wr a (off + 23) v263
      wr a (off + 24) v264
      wr a (off + 25) v265
      wr a (off + 26) v266
      wr a (off + 27) v267
      wr a (off + 28) v268
      wr a (off + 29) v269
      wr a (off + 30) v270
      wr a (off + 31) v271
  | otherwise = do
      let h = n `shiftR` 1
      forLoop 0 (h - 1) $ \i -> do
        rd a (off + 2 * i) >>= wr t (toff + i)
        rd a (off + 2 * i + 1) >>= wr t (toff + h + i)
      let acc j | j < 1 = pure ()
                | otherwise = do
                    p <- rd t (toff + h + j - 1)
                    v <- rd t (toff + h + j)
                    wr t (toff + h + j) (v + p)
                    acc (j - 1)
      acc (h - 1)
      v0 <- rd t (toff + h)
      wr t (toff + h) (v0 * sqrt2)
      idct1 t toff h a off
      idct1 t (toff + h) h a (off + h)
      forLoop 0 (h - 1) $ \i -> do
        e <- rd t (toff + i)
        o <- rd t (toff + h + i)
        let r = o * (secTab `unsafeAt` (h + i))
        wr a (off + i) (e + r)
        wr a (off + n - 1 - i) (e - r)

-- Inverse transform with a proven zero tail. Odd coefficient adjacency
-- creates one extra live value; propagate that fact through the recursion.
-- Small kernels retain their existing arithmetic graph.
idctPruned :: FPlane s -> Int -> Int -> Int -> FPlane s -> Int -> ST s ()
idctPruned a off n live t toff
  | live <= 1 = do
      dc <- rd a off
      forLoop 0 (n - 1) $ \i -> wr a (off + i) dc
  | n == 16 && live <= 4 = idct16x4 a off
  | n == 32 && live <= 4 = idct32x4 a off
  | n == 32 && live <= 8 = idct32x8 a off
  | n == 64 && live <= 8 = idct64x8 a off
  | n == 64 && live <= 16 = idct64x16 a off
  | n <= 32 || live > n `div` 2 = idct1 a off n t toff
  | otherwise = do
      let h = n `shiftR` 1
          ne = (live + 1) `shiftR` 1
          no = live `shiftR` 1
      forLoop 0 (h - 1) $ \i -> do
        e <- if i < ne then rd a (off + 2 * i) else pure 0
        o <- if i < no then rd a (off + 2 * i + 1) else pure 0
        wr t (toff + i) e
        wr t (toff + h + i) o
      let acc j | j < 1 = pure ()
                | otherwise = do
                    p <- rd t (toff + h + j - 1)
                    v <- rd t (toff + h + j)
                    wr t (toff + h + j) (v + p)
                    acc (j - 1)
      acc (min no (h - 1))
      v0 <- rd t (toff + h)
      wr t (toff + h) (v0 * sqrt2)
      idctPruned t toff h ne a off
      idctPruned t (toff + h) h (min h (no + 1)) a (off + h)
      forLoop 0 (h - 1) $ \i -> do
        e <- rd t (toff + i)
        o <- rd t (toff + h + i)
        let r = o * (secTab `unsafeAt` (h + i))
        wr a (off + i) (e + r)
        wr a (off + n - 1 - i) (e - r)

-- Straight-line radix-2 kernels with a proven zero tail, generated from
-- idct1's operation graph. Only operations involving known zeros disappear;
-- the order of every addition/multiplication of live values is preserved.
idct16x4 :: FPlane s -> Int -> ST s ()
idct16x4 a off = do
  i0 <- rd a (off + 0)
  i1 <- rd a (off + 1)
  i2 <- rd a (off + 2)
  i3 <- rd a (off + 3)
  let v0 = i2 * sqrt2
      v1 = i2 * sqrt2
      v2 = (v1 + i2) * 0.5411961
      v3 = (v1 - i2) * 1.306563
      v4 = v0 + v2
      v5 = v0 + v3
      v6 = v0 - v3
      v7 = v0 - v2
      v8 = v4 * 0.5097955791041592
      v9 = v5 * 0.6013448869350453
      v10 = v6 * 0.8999762231364156
      v11 = v7 * 2.5629154477415055
      v12 = i0 + v8
      v13 = i0 + v9
      v14 = i0 + v10
      v15 = i0 + v11
      v16 = i0 - v11
      v17 = i0 - v10
      v18 = i0 - v9
      v19 = i0 - v8
      v20 = i1 * sqrt2
      v21 = i3 + i1
      v22 = i3 * sqrt2
      v23 = (v22 + i3) * 0.5411961
      v24 = (v22 - i3) * 1.306563
      v25 = v20 + v23
      v26 = v20 + v24
      v27 = v20 - v24
      v28 = v20 - v23
      v29 = v21 * sqrt2
      v30 = v21 * sqrt2
      v31 = (v30 + v21) * 0.5411961
      v32 = (v30 - v21) * 1.306563
      v33 = v29 + v31
      v34 = v29 + v32
      v35 = v29 - v32
      v36 = v29 - v31
      v37 = v33 * 0.5097955791041592
      v38 = v34 * 0.6013448869350453
      v39 = v35 * 0.8999762231364156
      v40 = v36 * 2.5629154477415055
      v41 = v25 + v37
      v42 = v26 + v38
      v43 = v27 + v39
      v44 = v28 + v40
      v45 = v28 - v40
      v46 = v27 - v39
      v47 = v26 - v38
      v48 = v25 - v37
      v49 = v41 * 0.5024192861881557
      v50 = v42 * 0.5224986149396889
      v51 = v43 * 0.5669440348163577
      v52 = v44 * 0.6468217833599901
      v53 = v45 * 0.7881546234512502
      v54 = v46 * 1.060677685990347
      v55 = v47 * 1.7224470982383342
      v56 = v48 * 5.101148618689155
      v57 = v12 + v49
      v58 = v13 + v50
      v59 = v14 + v51
      v60 = v15 + v52
      v61 = v16 + v53
      v62 = v17 + v54
      v63 = v18 + v55
      v64 = v19 + v56
      v65 = v19 - v56
      v66 = v18 - v55
      v67 = v17 - v54
      v68 = v16 - v53
      v69 = v15 - v52
      v70 = v14 - v51
      v71 = v13 - v50
      v72 = v12 - v49
  wr a (off + 0) v57
  wr a (off + 1) v58
  wr a (off + 2) v59
  wr a (off + 3) v60
  wr a (off + 4) v61
  wr a (off + 5) v62
  wr a (off + 6) v63
  wr a (off + 7) v64
  wr a (off + 8) v65
  wr a (off + 9) v66
  wr a (off + 10) v67
  wr a (off + 11) v68
  wr a (off + 12) v69
  wr a (off + 13) v70
  wr a (off + 14) v71
  wr a (off + 15) v72

idct32x4 :: FPlane s -> Int -> ST s ()
idct32x4 a off = do
  i0 <- rd a (off + 0)
  i1 <- rd a (off + 1)
  i2 <- rd a (off + 2)
  i3 <- rd a (off + 3)
  let v0 = i2 * sqrt2
      v1 = i2 * sqrt2
      v2 = i2 * sqrt2
      v3 = (v2 + i2) * 0.5411961
      v4 = (v2 - i2) * 1.306563
      v5 = v1 + v3
      v6 = v1 + v4
      v7 = v1 - v4
      v8 = v1 - v3
      v9 = v5 * 0.5097955791041592
      v10 = v6 * 0.6013448869350453
      v11 = v7 * 0.8999762231364156
      v12 = v8 * 2.5629154477415055
      v13 = v0 + v9
      v14 = v0 + v10
      v15 = v0 + v11
      v16 = v0 + v12
      v17 = v0 - v12
      v18 = v0 - v11
      v19 = v0 - v10
      v20 = v0 - v9
      v21 = v13 * 0.5024192861881557
      v22 = v14 * 0.5224986149396889
      v23 = v15 * 0.5669440348163577
      v24 = v16 * 0.6468217833599901
      v25 = v17 * 0.7881546234512502
      v26 = v18 * 1.060677685990347
      v27 = v19 * 1.7224470982383342
      v28 = v20 * 5.101148618689155
      v29 = i0 + v21
      v30 = i0 + v22
      v31 = i0 + v23
      v32 = i0 + v24
      v33 = i0 + v25
      v34 = i0 + v26
      v35 = i0 + v27
      v36 = i0 + v28
      v37 = i0 - v28
      v38 = i0 - v27
      v39 = i0 - v26
      v40 = i0 - v25
      v41 = i0 - v24
      v42 = i0 - v23
      v43 = i0 - v22
      v44 = i0 - v21
      v45 = i1 * sqrt2
      v46 = i3 + i1
      v47 = i3 * sqrt2
      v48 = i3 * sqrt2
      v49 = (v48 + i3) * 0.5411961
      v50 = (v48 - i3) * 1.306563
      v51 = v47 + v49
      v52 = v47 + v50
      v53 = v47 - v50
      v54 = v47 - v49
      v55 = v51 * 0.5097955791041592
      v56 = v52 * 0.6013448869350453
      v57 = v53 * 0.8999762231364156
      v58 = v54 * 2.5629154477415055
      v59 = v45 + v55
      v60 = v45 + v56
      v61 = v45 + v57
      v62 = v45 + v58
      v63 = v45 - v58
      v64 = v45 - v57
      v65 = v45 - v56
      v66 = v45 - v55
      v67 = v46 * sqrt2
      v68 = v46 * sqrt2
      v69 = v46 * sqrt2
      v70 = (v69 + v46) * 0.5411961
      v71 = (v69 - v46) * 1.306563
      v72 = v68 + v70
      v73 = v68 + v71
      v74 = v68 - v71
      v75 = v68 - v70
      v76 = v72 * 0.5097955791041592
      v77 = v73 * 0.6013448869350453
      v78 = v74 * 0.8999762231364156
      v79 = v75 * 2.5629154477415055
      v80 = v67 + v76
      v81 = v67 + v77
      v82 = v67 + v78
      v83 = v67 + v79
      v84 = v67 - v79
      v85 = v67 - v78
      v86 = v67 - v77
      v87 = v67 - v76
      v88 = v80 * 0.5024192861881557
      v89 = v81 * 0.5224986149396889
      v90 = v82 * 0.5669440348163577
      v91 = v83 * 0.6468217833599901
      v92 = v84 * 0.7881546234512502
      v93 = v85 * 1.060677685990347
      v94 = v86 * 1.7224470982383342
      v95 = v87 * 5.101148618689155
      v96 = v59 + v88
      v97 = v60 + v89
      v98 = v61 + v90
      v99 = v62 + v91
      v100 = v63 + v92
      v101 = v64 + v93
      v102 = v65 + v94
      v103 = v66 + v95
      v104 = v66 - v95
      v105 = v65 - v94
      v106 = v64 - v93
      v107 = v63 - v92
      v108 = v62 - v91
      v109 = v61 - v90
      v110 = v60 - v89
      v111 = v59 - v88
      v112 = v96 * 0.5006029982351963
      v113 = v97 * 0.5054709598975436
      v114 = v98 * 0.5154473099226246
      v115 = v99 * 0.5310425910897841
      v116 = v100 * 0.5531038960344445
      v117 = v101 * 0.5829349682061339
      v118 = v102 * 0.6225041230356648
      v119 = v103 * 0.6748083414550057
      v120 = v104 * 0.7445362710022986
      v121 = v105 * 0.8393496454155268
      v122 = v106 * 0.9725682378619608
      v123 = v107 * 1.1694399334328847
      v124 = v108 * 1.4841646163141662
      v125 = v109 * 2.057781009953411
      v126 = v110 * 3.407608418468719
      v127 = v111 * 10.190008123548033
      v128 = v29 + v112
      v129 = v30 + v113
      v130 = v31 + v114
      v131 = v32 + v115
      v132 = v33 + v116
      v133 = v34 + v117
      v134 = v35 + v118
      v135 = v36 + v119
      v136 = v37 + v120
      v137 = v38 + v121
      v138 = v39 + v122
      v139 = v40 + v123
      v140 = v41 + v124
      v141 = v42 + v125
      v142 = v43 + v126
      v143 = v44 + v127
      v144 = v44 - v127
      v145 = v43 - v126
      v146 = v42 - v125
      v147 = v41 - v124
      v148 = v40 - v123
      v149 = v39 - v122
      v150 = v38 - v121
      v151 = v37 - v120
      v152 = v36 - v119
      v153 = v35 - v118
      v154 = v34 - v117
      v155 = v33 - v116
      v156 = v32 - v115
      v157 = v31 - v114
      v158 = v30 - v113
      v159 = v29 - v112
  wr a (off + 0) v128
  wr a (off + 1) v129
  wr a (off + 2) v130
  wr a (off + 3) v131
  wr a (off + 4) v132
  wr a (off + 5) v133
  wr a (off + 6) v134
  wr a (off + 7) v135
  wr a (off + 8) v136
  wr a (off + 9) v137
  wr a (off + 10) v138
  wr a (off + 11) v139
  wr a (off + 12) v140
  wr a (off + 13) v141
  wr a (off + 14) v142
  wr a (off + 15) v143
  wr a (off + 16) v144
  wr a (off + 17) v145
  wr a (off + 18) v146
  wr a (off + 19) v147
  wr a (off + 20) v148
  wr a (off + 21) v149
  wr a (off + 22) v150
  wr a (off + 23) v151
  wr a (off + 24) v152
  wr a (off + 25) v153
  wr a (off + 26) v154
  wr a (off + 27) v155
  wr a (off + 28) v156
  wr a (off + 29) v157
  wr a (off + 30) v158
  wr a (off + 31) v159

idct32x8 :: FPlane s -> Int -> ST s ()
idct32x8 a off = do
  i0 <- rd a (off + 0)
  i1 <- rd a (off + 1)
  i2 <- rd a (off + 2)
  i3 <- rd a (off + 3)
  i4 <- rd a (off + 4)
  i5 <- rd a (off + 5)
  i6 <- rd a (off + 6)
  i7 <- rd a (off + 7)
  let v0 = i4 * sqrt2
      v1 = i4 * sqrt2
      v2 = (v1 + i4) * 0.5411961
      v3 = (v1 - i4) * 1.306563
      v4 = v0 + v2
      v5 = v0 + v3
      v6 = v0 - v3
      v7 = v0 - v2
      v8 = v4 * 0.5097955791041592
      v9 = v5 * 0.6013448869350453
      v10 = v6 * 0.8999762231364156
      v11 = v7 * 2.5629154477415055
      v12 = i0 + v8
      v13 = i0 + v9
      v14 = i0 + v10
      v15 = i0 + v11
      v16 = i0 - v11
      v17 = i0 - v10
      v18 = i0 - v9
      v19 = i0 - v8
      v20 = i2 * sqrt2
      v21 = i6 + i2
      v22 = i6 * sqrt2
      v23 = (v22 + i6) * 0.5411961
      v24 = (v22 - i6) * 1.306563
      v25 = v20 + v23
      v26 = v20 + v24
      v27 = v20 - v24
      v28 = v20 - v23
      v29 = v21 * sqrt2
      v30 = v21 * sqrt2
      v31 = (v30 + v21) * 0.5411961
      v32 = (v30 - v21) * 1.306563
      v33 = v29 + v31
      v34 = v29 + v32
      v35 = v29 - v32
      v36 = v29 - v31
      v37 = v33 * 0.5097955791041592
      v38 = v34 * 0.6013448869350453
      v39 = v35 * 0.8999762231364156
      v40 = v36 * 2.5629154477415055
      v41 = v25 + v37
      v42 = v26 + v38
      v43 = v27 + v39
      v44 = v28 + v40
      v45 = v28 - v40
      v46 = v27 - v39
      v47 = v26 - v38
      v48 = v25 - v37
      v49 = v41 * 0.5024192861881557
      v50 = v42 * 0.5224986149396889
      v51 = v43 * 0.5669440348163577
      v52 = v44 * 0.6468217833599901
      v53 = v45 * 0.7881546234512502
      v54 = v46 * 1.060677685990347
      v55 = v47 * 1.7224470982383342
      v56 = v48 * 5.101148618689155
      v57 = v12 + v49
      v58 = v13 + v50
      v59 = v14 + v51
      v60 = v15 + v52
      v61 = v16 + v53
      v62 = v17 + v54
      v63 = v18 + v55
      v64 = v19 + v56
      v65 = v19 - v56
      v66 = v18 - v55
      v67 = v17 - v54
      v68 = v16 - v53
      v69 = v15 - v52
      v70 = v14 - v51
      v71 = v13 - v50
      v72 = v12 - v49
      v73 = i1 * sqrt2
      v74 = i3 + i1
      v75 = i5 + i3
      v76 = i7 + i5
      v77 = i7 * sqrt2
      v78 = (v77 + i7) * 0.5411961
      v79 = (v77 - i7) * 1.306563
      v80 = v73 + v78
      v81 = v73 + v79
      v82 = v73 - v79
      v83 = v73 - v78
      v84 = v75 * sqrt2
      v85 = v75 * sqrt2
      v86 = (v85 + v75) * 0.5411961
      v87 = (v85 - v75) * 1.306563
      v88 = v84 + v86
      v89 = v84 + v87
      v90 = v84 - v87
      v91 = v84 - v86
      v92 = v88 * 0.5097955791041592
      v93 = v89 * 0.6013448869350453
      v94 = v90 * 0.8999762231364156
      v95 = v91 * 2.5629154477415055
      v96 = v80 + v92
      v97 = v81 + v93
      v98 = v82 + v94
      v99 = v83 + v95
      v100 = v83 - v95
      v101 = v82 - v94
      v102 = v81 - v93
      v103 = v80 - v92
      v104 = v74 * sqrt2
      v105 = v76 + v74
      v106 = v76 * sqrt2
      v107 = (v106 + v76) * 0.5411961
      v108 = (v106 - v76) * 1.306563
      v109 = v104 + v107
      v110 = v104 + v108
      v111 = v104 - v108
      v112 = v104 - v107
      v113 = v105 * sqrt2
      v114 = v105 * sqrt2
      v115 = (v114 + v105) * 0.5411961
      v116 = (v114 - v105) * 1.306563
      v117 = v113 + v115
      v118 = v113 + v116
      v119 = v113 - v116
      v120 = v113 - v115
      v121 = v117 * 0.5097955791041592
      v122 = v118 * 0.6013448869350453
      v123 = v119 * 0.8999762231364156
      v124 = v120 * 2.5629154477415055
      v125 = v109 + v121
      v126 = v110 + v122
      v127 = v111 + v123
      v128 = v112 + v124
      v129 = v112 - v124
      v130 = v111 - v123
      v131 = v110 - v122
      v132 = v109 - v121
      v133 = v125 * 0.5024192861881557
      v134 = v126 * 0.5224986149396889
      v135 = v127 * 0.5669440348163577
      v136 = v128 * 0.6468217833599901
      v137 = v129 * 0.7881546234512502
      v138 = v130 * 1.060677685990347
      v139 = v131 * 1.7224470982383342
      v140 = v132 * 5.101148618689155
      v141 = v96 + v133
      v142 = v97 + v134
      v143 = v98 + v135
      v144 = v99 + v136
      v145 = v100 + v137
      v146 = v101 + v138
      v147 = v102 + v139
      v148 = v103 + v140
      v149 = v103 - v140
      v150 = v102 - v139
      v151 = v101 - v138
      v152 = v100 - v137
      v153 = v99 - v136
      v154 = v98 - v135
      v155 = v97 - v134
      v156 = v96 - v133
      v157 = v141 * 0.5006029982351963
      v158 = v142 * 0.5054709598975436
      v159 = v143 * 0.5154473099226246
      v160 = v144 * 0.5310425910897841
      v161 = v145 * 0.5531038960344445
      v162 = v146 * 0.5829349682061339
      v163 = v147 * 0.6225041230356648
      v164 = v148 * 0.6748083414550057
      v165 = v149 * 0.7445362710022986
      v166 = v150 * 0.8393496454155268
      v167 = v151 * 0.9725682378619608
      v168 = v152 * 1.1694399334328847
      v169 = v153 * 1.4841646163141662
      v170 = v154 * 2.057781009953411
      v171 = v155 * 3.407608418468719
      v172 = v156 * 10.190008123548033
      v173 = v57 + v157
      v174 = v58 + v158
      v175 = v59 + v159
      v176 = v60 + v160
      v177 = v61 + v161
      v178 = v62 + v162
      v179 = v63 + v163
      v180 = v64 + v164
      v181 = v65 + v165
      v182 = v66 + v166
      v183 = v67 + v167
      v184 = v68 + v168
      v185 = v69 + v169
      v186 = v70 + v170
      v187 = v71 + v171
      v188 = v72 + v172
      v189 = v72 - v172
      v190 = v71 - v171
      v191 = v70 - v170
      v192 = v69 - v169
      v193 = v68 - v168
      v194 = v67 - v167
      v195 = v66 - v166
      v196 = v65 - v165
      v197 = v64 - v164
      v198 = v63 - v163
      v199 = v62 - v162
      v200 = v61 - v161
      v201 = v60 - v160
      v202 = v59 - v159
      v203 = v58 - v158
      v204 = v57 - v157
  wr a (off + 0) v173
  wr a (off + 1) v174
  wr a (off + 2) v175
  wr a (off + 3) v176
  wr a (off + 4) v177
  wr a (off + 5) v178
  wr a (off + 6) v179
  wr a (off + 7) v180
  wr a (off + 8) v181
  wr a (off + 9) v182
  wr a (off + 10) v183
  wr a (off + 11) v184
  wr a (off + 12) v185
  wr a (off + 13) v186
  wr a (off + 14) v187
  wr a (off + 15) v188
  wr a (off + 16) v189
  wr a (off + 17) v190
  wr a (off + 18) v191
  wr a (off + 19) v192
  wr a (off + 20) v193
  wr a (off + 21) v194
  wr a (off + 22) v195
  wr a (off + 23) v196
  wr a (off + 24) v197
  wr a (off + 25) v198
  wr a (off + 26) v199
  wr a (off + 27) v200
  wr a (off + 28) v201
  wr a (off + 29) v202
  wr a (off + 30) v203
  wr a (off + 31) v204

idct64x8 :: FPlane s -> Int -> ST s ()
idct64x8 a off = do
  i0 <- rd a (off + 0)
  i1 <- rd a (off + 1)
  i2 <- rd a (off + 2)
  i3 <- rd a (off + 3)
  i4 <- rd a (off + 4)
  i5 <- rd a (off + 5)
  i6 <- rd a (off + 6)
  i7 <- rd a (off + 7)
  let v0 = i4 * sqrt2
      v1 = i4 * sqrt2
      v2 = i4 * sqrt2
      v3 = (v2 + i4) * 0.5411961
      v4 = (v2 - i4) * 1.306563
      v5 = v1 + v3
      v6 = v1 + v4
      v7 = v1 - v4
      v8 = v1 - v3
      v9 = v5 * 0.5097955791041592
      v10 = v6 * 0.6013448869350453
      v11 = v7 * 0.8999762231364156
      v12 = v8 * 2.5629154477415055
      v13 = v0 + v9
      v14 = v0 + v10
      v15 = v0 + v11
      v16 = v0 + v12
      v17 = v0 - v12
      v18 = v0 - v11
      v19 = v0 - v10
      v20 = v0 - v9
      v21 = v13 * 0.5024192861881557
      v22 = v14 * 0.5224986149396889
      v23 = v15 * 0.5669440348163577
      v24 = v16 * 0.6468217833599901
      v25 = v17 * 0.7881546234512502
      v26 = v18 * 1.060677685990347
      v27 = v19 * 1.7224470982383342
      v28 = v20 * 5.101148618689155
      v29 = i0 + v21
      v30 = i0 + v22
      v31 = i0 + v23
      v32 = i0 + v24
      v33 = i0 + v25
      v34 = i0 + v26
      v35 = i0 + v27
      v36 = i0 + v28
      v37 = i0 - v28
      v38 = i0 - v27
      v39 = i0 - v26
      v40 = i0 - v25
      v41 = i0 - v24
      v42 = i0 - v23
      v43 = i0 - v22
      v44 = i0 - v21
      v45 = i2 * sqrt2
      v46 = i6 + i2
      v47 = i6 * sqrt2
      v48 = i6 * sqrt2
      v49 = (v48 + i6) * 0.5411961
      v50 = (v48 - i6) * 1.306563
      v51 = v47 + v49
      v52 = v47 + v50
      v53 = v47 - v50
      v54 = v47 - v49
      v55 = v51 * 0.5097955791041592
      v56 = v52 * 0.6013448869350453
      v57 = v53 * 0.8999762231364156
      v58 = v54 * 2.5629154477415055
      v59 = v45 + v55
      v60 = v45 + v56
      v61 = v45 + v57
      v62 = v45 + v58
      v63 = v45 - v58
      v64 = v45 - v57
      v65 = v45 - v56
      v66 = v45 - v55
      v67 = v46 * sqrt2
      v68 = v46 * sqrt2
      v69 = v46 * sqrt2
      v70 = (v69 + v46) * 0.5411961
      v71 = (v69 - v46) * 1.306563
      v72 = v68 + v70
      v73 = v68 + v71
      v74 = v68 - v71
      v75 = v68 - v70
      v76 = v72 * 0.5097955791041592
      v77 = v73 * 0.6013448869350453
      v78 = v74 * 0.8999762231364156
      v79 = v75 * 2.5629154477415055
      v80 = v67 + v76
      v81 = v67 + v77
      v82 = v67 + v78
      v83 = v67 + v79
      v84 = v67 - v79
      v85 = v67 - v78
      v86 = v67 - v77
      v87 = v67 - v76
      v88 = v80 * 0.5024192861881557
      v89 = v81 * 0.5224986149396889
      v90 = v82 * 0.5669440348163577
      v91 = v83 * 0.6468217833599901
      v92 = v84 * 0.7881546234512502
      v93 = v85 * 1.060677685990347
      v94 = v86 * 1.7224470982383342
      v95 = v87 * 5.101148618689155
      v96 = v59 + v88
      v97 = v60 + v89
      v98 = v61 + v90
      v99 = v62 + v91
      v100 = v63 + v92
      v101 = v64 + v93
      v102 = v65 + v94
      v103 = v66 + v95
      v104 = v66 - v95
      v105 = v65 - v94
      v106 = v64 - v93
      v107 = v63 - v92
      v108 = v62 - v91
      v109 = v61 - v90
      v110 = v60 - v89
      v111 = v59 - v88
      v112 = v96 * 0.5006029982351963
      v113 = v97 * 0.5054709598975436
      v114 = v98 * 0.5154473099226246
      v115 = v99 * 0.5310425910897841
      v116 = v100 * 0.5531038960344445
      v117 = v101 * 0.5829349682061339
      v118 = v102 * 0.6225041230356648
      v119 = v103 * 0.6748083414550057
      v120 = v104 * 0.7445362710022986
      v121 = v105 * 0.8393496454155268
      v122 = v106 * 0.9725682378619608
      v123 = v107 * 1.1694399334328847
      v124 = v108 * 1.4841646163141662
      v125 = v109 * 2.057781009953411
      v126 = v110 * 3.407608418468719
      v127 = v111 * 10.190008123548033
      v128 = v29 + v112
      v129 = v30 + v113
      v130 = v31 + v114
      v131 = v32 + v115
      v132 = v33 + v116
      v133 = v34 + v117
      v134 = v35 + v118
      v135 = v36 + v119
      v136 = v37 + v120
      v137 = v38 + v121
      v138 = v39 + v122
      v139 = v40 + v123
      v140 = v41 + v124
      v141 = v42 + v125
      v142 = v43 + v126
      v143 = v44 + v127
      v144 = v44 - v127
      v145 = v43 - v126
      v146 = v42 - v125
      v147 = v41 - v124
      v148 = v40 - v123
      v149 = v39 - v122
      v150 = v38 - v121
      v151 = v37 - v120
      v152 = v36 - v119
      v153 = v35 - v118
      v154 = v34 - v117
      v155 = v33 - v116
      v156 = v32 - v115
      v157 = v31 - v114
      v158 = v30 - v113
      v159 = v29 - v112
      v160 = i1 * sqrt2
      v161 = i3 + i1
      v162 = i5 + i3
      v163 = i7 + i5
      v164 = i7 * sqrt2
      v165 = i7 * sqrt2
      v166 = (v165 + i7) * 0.5411961
      v167 = (v165 - i7) * 1.306563
      v168 = v164 + v166
      v169 = v164 + v167
      v170 = v164 - v167
      v171 = v164 - v166
      v172 = v168 * 0.5097955791041592
      v173 = v169 * 0.6013448869350453
      v174 = v170 * 0.8999762231364156
      v175 = v171 * 2.5629154477415055
      v176 = v160 + v172
      v177 = v160 + v173
      v178 = v160 + v174
      v179 = v160 + v175
      v180 = v160 - v175
      v181 = v160 - v174
      v182 = v160 - v173
      v183 = v160 - v172
      v184 = v162 * sqrt2
      v185 = v162 * sqrt2
      v186 = v162 * sqrt2
      v187 = (v186 + v162) * 0.5411961
      v188 = (v186 - v162) * 1.306563
      v189 = v185 + v187
      v190 = v185 + v188
      v191 = v185 - v188
      v192 = v185 - v187
      v193 = v189 * 0.5097955791041592
      v194 = v190 * 0.6013448869350453
      v195 = v191 * 0.8999762231364156
      v196 = v192 * 2.5629154477415055
      v197 = v184 + v193
      v198 = v184 + v194
      v199 = v184 + v195
      v200 = v184 + v196
      v201 = v184 - v196
      v202 = v184 - v195
      v203 = v184 - v194
      v204 = v184 - v193
      v205 = v197 * 0.5024192861881557
      v206 = v198 * 0.5224986149396889
      v207 = v199 * 0.5669440348163577
      v208 = v200 * 0.6468217833599901
      v209 = v201 * 0.7881546234512502
      v210 = v202 * 1.060677685990347
      v211 = v203 * 1.7224470982383342
      v212 = v204 * 5.101148618689155
      v213 = v176 + v205
      v214 = v177 + v206
      v215 = v178 + v207
      v216 = v179 + v208
      v217 = v180 + v209
      v218 = v181 + v210
      v219 = v182 + v211
      v220 = v183 + v212
      v221 = v183 - v212
      v222 = v182 - v211
      v223 = v181 - v210
      v224 = v180 - v209
      v225 = v179 - v208
      v226 = v178 - v207
      v227 = v177 - v206
      v228 = v176 - v205
      v229 = v161 * sqrt2
      v230 = v163 + v161
      v231 = v163 * sqrt2
      v232 = v163 * sqrt2
      v233 = (v232 + v163) * 0.5411961
      v234 = (v232 - v163) * 1.306563
      v235 = v231 + v233
      v236 = v231 + v234
      v237 = v231 - v234
      v238 = v231 - v233
      v239 = v235 * 0.5097955791041592
      v240 = v236 * 0.6013448869350453
      v241 = v237 * 0.8999762231364156
      v242 = v238 * 2.5629154477415055
      v243 = v229 + v239
      v244 = v229 + v240
      v245 = v229 + v241
      v246 = v229 + v242
      v247 = v229 - v242
      v248 = v229 - v241
      v249 = v229 - v240
      v250 = v229 - v239
      v251 = v230 * sqrt2
      v252 = v230 * sqrt2
      v253 = v230 * sqrt2
      v254 = (v253 + v230) * 0.5411961
      v255 = (v253 - v230) * 1.306563
      v256 = v252 + v254
      v257 = v252 + v255
      v258 = v252 - v255
      v259 = v252 - v254
      v260 = v256 * 0.5097955791041592
      v261 = v257 * 0.6013448869350453
      v262 = v258 * 0.8999762231364156
      v263 = v259 * 2.5629154477415055
      v264 = v251 + v260
      v265 = v251 + v261
      v266 = v251 + v262
      v267 = v251 + v263
      v268 = v251 - v263
      v269 = v251 - v262
      v270 = v251 - v261
      v271 = v251 - v260
      v272 = v264 * 0.5024192861881557
      v273 = v265 * 0.5224986149396889
      v274 = v266 * 0.5669440348163577
      v275 = v267 * 0.6468217833599901
      v276 = v268 * 0.7881546234512502
      v277 = v269 * 1.060677685990347
      v278 = v270 * 1.7224470982383342
      v279 = v271 * 5.101148618689155
      v280 = v243 + v272
      v281 = v244 + v273
      v282 = v245 + v274
      v283 = v246 + v275
      v284 = v247 + v276
      v285 = v248 + v277
      v286 = v249 + v278
      v287 = v250 + v279
      v288 = v250 - v279
      v289 = v249 - v278
      v290 = v248 - v277
      v291 = v247 - v276
      v292 = v246 - v275
      v293 = v245 - v274
      v294 = v244 - v273
      v295 = v243 - v272
      v296 = v280 * 0.5006029982351963
      v297 = v281 * 0.5054709598975436
      v298 = v282 * 0.5154473099226246
      v299 = v283 * 0.5310425910897841
      v300 = v284 * 0.5531038960344445
      v301 = v285 * 0.5829349682061339
      v302 = v286 * 0.6225041230356648
      v303 = v287 * 0.6748083414550057
      v304 = v288 * 0.7445362710022986
      v305 = v289 * 0.8393496454155268
      v306 = v290 * 0.9725682378619608
      v307 = v291 * 1.1694399334328847
      v308 = v292 * 1.4841646163141662
      v309 = v293 * 2.057781009953411
      v310 = v294 * 3.407608418468719
      v311 = v295 * 10.190008123548033
      v312 = v213 + v296
      v313 = v214 + v297
      v314 = v215 + v298
      v315 = v216 + v299
      v316 = v217 + v300
      v317 = v218 + v301
      v318 = v219 + v302
      v319 = v220 + v303
      v320 = v221 + v304
      v321 = v222 + v305
      v322 = v223 + v306
      v323 = v224 + v307
      v324 = v225 + v308
      v325 = v226 + v309
      v326 = v227 + v310
      v327 = v228 + v311
      v328 = v228 - v311
      v329 = v227 - v310
      v330 = v226 - v309
      v331 = v225 - v308
      v332 = v224 - v307
      v333 = v223 - v306
      v334 = v222 - v305
      v335 = v221 - v304
      v336 = v220 - v303
      v337 = v219 - v302
      v338 = v218 - v301
      v339 = v217 - v300
      v340 = v216 - v299
      v341 = v215 - v298
      v342 = v214 - v297
      v343 = v213 - v296
      v344 = v312 * 0.500150636020651
      v345 = v313 * 0.5013584524464084
      v346 = v314 * 0.5037887256810443
      v347 = v315 * 0.5074711720725553
      v348 = v316 * 0.5124514794082247
      v349 = v317 * 0.5187927131053328
      v350 = v318 * 0.52657731515427
      v351 = v319 * 0.535909816907992
      v352 = v320 * 0.5469204379855088
      v353 = v321 * 0.5597698129470802
      v354 = v322 * 0.57465518403266
      v355 = v323 * 0.5918185358574165
      v356 = v324 * 0.6115573478825099
      v357 = v325 * 0.6342389366884031
      v358 = v326 * 0.6603198078137061
      v359 = v327 * 0.6903721282002123
      v360 = v328 * 0.7251205223771985
      v361 = v329 * 0.7654941649730891
      v362 = v330 * 0.8127020908144905
      v363 = v331 * 0.8683447152233481
      v364 = v332 * 0.9345835970364075
      v365 = v333 * 1.0144082649970547
      v366 = v334 * 1.1120716205797176
      v367 = v335 * 1.233832737976571
      v368 = v336 * 1.3892939586328277
      v369 = v337 * 1.5939722833856311
      v370 = v338 * 1.8746759800084078
      v371 = v339 * 2.282050068005162
      v372 = v340 * 2.924628428158216
      v373 = v341 * 4.084611078129248
      v374 = v342 * 6.796750711673633
      v375 = v343 * 20.373878167231453
      v376 = v128 + v344
      v377 = v129 + v345
      v378 = v130 + v346
      v379 = v131 + v347
      v380 = v132 + v348
      v381 = v133 + v349
      v382 = v134 + v350
      v383 = v135 + v351
      v384 = v136 + v352
      v385 = v137 + v353
      v386 = v138 + v354
      v387 = v139 + v355
      v388 = v140 + v356
      v389 = v141 + v357
      v390 = v142 + v358
      v391 = v143 + v359
      v392 = v144 + v360
      v393 = v145 + v361
      v394 = v146 + v362
      v395 = v147 + v363
      v396 = v148 + v364
      v397 = v149 + v365
      v398 = v150 + v366
      v399 = v151 + v367
      v400 = v152 + v368
      v401 = v153 + v369
      v402 = v154 + v370
      v403 = v155 + v371
      v404 = v156 + v372
      v405 = v157 + v373
      v406 = v158 + v374
      v407 = v159 + v375
      v408 = v159 - v375
      v409 = v158 - v374
      v410 = v157 - v373
      v411 = v156 - v372
      v412 = v155 - v371
      v413 = v154 - v370
      v414 = v153 - v369
      v415 = v152 - v368
      v416 = v151 - v367
      v417 = v150 - v366
      v418 = v149 - v365
      v419 = v148 - v364
      v420 = v147 - v363
      v421 = v146 - v362
      v422 = v145 - v361
      v423 = v144 - v360
      v424 = v143 - v359
      v425 = v142 - v358
      v426 = v141 - v357
      v427 = v140 - v356
      v428 = v139 - v355
      v429 = v138 - v354
      v430 = v137 - v353
      v431 = v136 - v352
      v432 = v135 - v351
      v433 = v134 - v350
      v434 = v133 - v349
      v435 = v132 - v348
      v436 = v131 - v347
      v437 = v130 - v346
      v438 = v129 - v345
      v439 = v128 - v344
  wr a (off + 0) v376
  wr a (off + 1) v377
  wr a (off + 2) v378
  wr a (off + 3) v379
  wr a (off + 4) v380
  wr a (off + 5) v381
  wr a (off + 6) v382
  wr a (off + 7) v383
  wr a (off + 8) v384
  wr a (off + 9) v385
  wr a (off + 10) v386
  wr a (off + 11) v387
  wr a (off + 12) v388
  wr a (off + 13) v389
  wr a (off + 14) v390
  wr a (off + 15) v391
  wr a (off + 16) v392
  wr a (off + 17) v393
  wr a (off + 18) v394
  wr a (off + 19) v395
  wr a (off + 20) v396
  wr a (off + 21) v397
  wr a (off + 22) v398
  wr a (off + 23) v399
  wr a (off + 24) v400
  wr a (off + 25) v401
  wr a (off + 26) v402
  wr a (off + 27) v403
  wr a (off + 28) v404
  wr a (off + 29) v405
  wr a (off + 30) v406
  wr a (off + 31) v407
  wr a (off + 32) v408
  wr a (off + 33) v409
  wr a (off + 34) v410
  wr a (off + 35) v411
  wr a (off + 36) v412
  wr a (off + 37) v413
  wr a (off + 38) v414
  wr a (off + 39) v415
  wr a (off + 40) v416
  wr a (off + 41) v417
  wr a (off + 42) v418
  wr a (off + 43) v419
  wr a (off + 44) v420
  wr a (off + 45) v421
  wr a (off + 46) v422
  wr a (off + 47) v423
  wr a (off + 48) v424
  wr a (off + 49) v425
  wr a (off + 50) v426
  wr a (off + 51) v427
  wr a (off + 52) v428
  wr a (off + 53) v429
  wr a (off + 54) v430
  wr a (off + 55) v431
  wr a (off + 56) v432
  wr a (off + 57) v433
  wr a (off + 58) v434
  wr a (off + 59) v435
  wr a (off + 60) v436
  wr a (off + 61) v437
  wr a (off + 62) v438
  wr a (off + 63) v439

idct64x16 :: FPlane s -> Int -> ST s ()
idct64x16 a off = do
  i0 <- rd a (off + 0)
  i1 <- rd a (off + 1)
  i2 <- rd a (off + 2)
  i3 <- rd a (off + 3)
  i4 <- rd a (off + 4)
  i5 <- rd a (off + 5)
  i6 <- rd a (off + 6)
  i7 <- rd a (off + 7)
  i8 <- rd a (off + 8)
  i9 <- rd a (off + 9)
  i10 <- rd a (off + 10)
  i11 <- rd a (off + 11)
  i12 <- rd a (off + 12)
  i13 <- rd a (off + 13)
  i14 <- rd a (off + 14)
  i15 <- rd a (off + 15)
  let v0 = i8 * sqrt2
      v1 = i8 * sqrt2
      v2 = (v1 + i8) * 0.5411961
      v3 = (v1 - i8) * 1.306563
      v4 = v0 + v2
      v5 = v0 + v3
      v6 = v0 - v3
      v7 = v0 - v2
      v8 = v4 * 0.5097955791041592
      v9 = v5 * 0.6013448869350453
      v10 = v6 * 0.8999762231364156
      v11 = v7 * 2.5629154477415055
      v12 = i0 + v8
      v13 = i0 + v9
      v14 = i0 + v10
      v15 = i0 + v11
      v16 = i0 - v11
      v17 = i0 - v10
      v18 = i0 - v9
      v19 = i0 - v8
      v20 = i4 * sqrt2
      v21 = i12 + i4
      v22 = i12 * sqrt2
      v23 = (v22 + i12) * 0.5411961
      v24 = (v22 - i12) * 1.306563
      v25 = v20 + v23
      v26 = v20 + v24
      v27 = v20 - v24
      v28 = v20 - v23
      v29 = v21 * sqrt2
      v30 = v21 * sqrt2
      v31 = (v30 + v21) * 0.5411961
      v32 = (v30 - v21) * 1.306563
      v33 = v29 + v31
      v34 = v29 + v32
      v35 = v29 - v32
      v36 = v29 - v31
      v37 = v33 * 0.5097955791041592
      v38 = v34 * 0.6013448869350453
      v39 = v35 * 0.8999762231364156
      v40 = v36 * 2.5629154477415055
      v41 = v25 + v37
      v42 = v26 + v38
      v43 = v27 + v39
      v44 = v28 + v40
      v45 = v28 - v40
      v46 = v27 - v39
      v47 = v26 - v38
      v48 = v25 - v37
      v49 = v41 * 0.5024192861881557
      v50 = v42 * 0.5224986149396889
      v51 = v43 * 0.5669440348163577
      v52 = v44 * 0.6468217833599901
      v53 = v45 * 0.7881546234512502
      v54 = v46 * 1.060677685990347
      v55 = v47 * 1.7224470982383342
      v56 = v48 * 5.101148618689155
      v57 = v12 + v49
      v58 = v13 + v50
      v59 = v14 + v51
      v60 = v15 + v52
      v61 = v16 + v53
      v62 = v17 + v54
      v63 = v18 + v55
      v64 = v19 + v56
      v65 = v19 - v56
      v66 = v18 - v55
      v67 = v17 - v54
      v68 = v16 - v53
      v69 = v15 - v52
      v70 = v14 - v51
      v71 = v13 - v50
      v72 = v12 - v49
      v73 = i2 * sqrt2
      v74 = i6 + i2
      v75 = i10 + i6
      v76 = i14 + i10
      v77 = i14 * sqrt2
      v78 = (v77 + i14) * 0.5411961
      v79 = (v77 - i14) * 1.306563
      v80 = v73 + v78
      v81 = v73 + v79
      v82 = v73 - v79
      v83 = v73 - v78
      v84 = v75 * sqrt2
      v85 = v75 * sqrt2
      v86 = (v85 + v75) * 0.5411961
      v87 = (v85 - v75) * 1.306563
      v88 = v84 + v86
      v89 = v84 + v87
      v90 = v84 - v87
      v91 = v84 - v86
      v92 = v88 * 0.5097955791041592
      v93 = v89 * 0.6013448869350453
      v94 = v90 * 0.8999762231364156
      v95 = v91 * 2.5629154477415055
      v96 = v80 + v92
      v97 = v81 + v93
      v98 = v82 + v94
      v99 = v83 + v95
      v100 = v83 - v95
      v101 = v82 - v94
      v102 = v81 - v93
      v103 = v80 - v92
      v104 = v74 * sqrt2
      v105 = v76 + v74
      v106 = v76 * sqrt2
      v107 = (v106 + v76) * 0.5411961
      v108 = (v106 - v76) * 1.306563
      v109 = v104 + v107
      v110 = v104 + v108
      v111 = v104 - v108
      v112 = v104 - v107
      v113 = v105 * sqrt2
      v114 = v105 * sqrt2
      v115 = (v114 + v105) * 0.5411961
      v116 = (v114 - v105) * 1.306563
      v117 = v113 + v115
      v118 = v113 + v116
      v119 = v113 - v116
      v120 = v113 - v115
      v121 = v117 * 0.5097955791041592
      v122 = v118 * 0.6013448869350453
      v123 = v119 * 0.8999762231364156
      v124 = v120 * 2.5629154477415055
      v125 = v109 + v121
      v126 = v110 + v122
      v127 = v111 + v123
      v128 = v112 + v124
      v129 = v112 - v124
      v130 = v111 - v123
      v131 = v110 - v122
      v132 = v109 - v121
      v133 = v125 * 0.5024192861881557
      v134 = v126 * 0.5224986149396889
      v135 = v127 * 0.5669440348163577
      v136 = v128 * 0.6468217833599901
      v137 = v129 * 0.7881546234512502
      v138 = v130 * 1.060677685990347
      v139 = v131 * 1.7224470982383342
      v140 = v132 * 5.101148618689155
      v141 = v96 + v133
      v142 = v97 + v134
      v143 = v98 + v135
      v144 = v99 + v136
      v145 = v100 + v137
      v146 = v101 + v138
      v147 = v102 + v139
      v148 = v103 + v140
      v149 = v103 - v140
      v150 = v102 - v139
      v151 = v101 - v138
      v152 = v100 - v137
      v153 = v99 - v136
      v154 = v98 - v135
      v155 = v97 - v134
      v156 = v96 - v133
      v157 = v141 * 0.5006029982351963
      v158 = v142 * 0.5054709598975436
      v159 = v143 * 0.5154473099226246
      v160 = v144 * 0.5310425910897841
      v161 = v145 * 0.5531038960344445
      v162 = v146 * 0.5829349682061339
      v163 = v147 * 0.6225041230356648
      v164 = v148 * 0.6748083414550057
      v165 = v149 * 0.7445362710022986
      v166 = v150 * 0.8393496454155268
      v167 = v151 * 0.9725682378619608
      v168 = v152 * 1.1694399334328847
      v169 = v153 * 1.4841646163141662
      v170 = v154 * 2.057781009953411
      v171 = v155 * 3.407608418468719
      v172 = v156 * 10.190008123548033
      v173 = v57 + v157
      v174 = v58 + v158
      v175 = v59 + v159
      v176 = v60 + v160
      v177 = v61 + v161
      v178 = v62 + v162
      v179 = v63 + v163
      v180 = v64 + v164
      v181 = v65 + v165
      v182 = v66 + v166
      v183 = v67 + v167
      v184 = v68 + v168
      v185 = v69 + v169
      v186 = v70 + v170
      v187 = v71 + v171
      v188 = v72 + v172
      v189 = v72 - v172
      v190 = v71 - v171
      v191 = v70 - v170
      v192 = v69 - v169
      v193 = v68 - v168
      v194 = v67 - v167
      v195 = v66 - v166
      v196 = v65 - v165
      v197 = v64 - v164
      v198 = v63 - v163
      v199 = v62 - v162
      v200 = v61 - v161
      v201 = v60 - v160
      v202 = v59 - v159
      v203 = v58 - v158
      v204 = v57 - v157
      v205 = i1 * sqrt2
      v206 = i3 + i1
      v207 = i5 + i3
      v208 = i7 + i5
      v209 = i9 + i7
      v210 = i11 + i9
      v211 = i13 + i11
      v212 = i15 + i13
      v213 = i15 * sqrt2
      v214 = (v213 + i15) * 0.5411961
      v215 = (v213 - i15) * 1.306563
      v216 = v205 + v214
      v217 = v205 + v215
      v218 = v205 - v215
      v219 = v205 - v214
      v220 = v209 * sqrt2
      v221 = v209 * sqrt2
      v222 = (v221 + v209) * 0.5411961
      v223 = (v221 - v209) * 1.306563
      v224 = v220 + v222
      v225 = v220 + v223
      v226 = v220 - v223
      v227 = v220 - v222
      v228 = v224 * 0.5097955791041592
      v229 = v225 * 0.6013448869350453
      v230 = v226 * 0.8999762231364156
      v231 = v227 * 2.5629154477415055
      v232 = v216 + v228
      v233 = v217 + v229
      v234 = v218 + v230
      v235 = v219 + v231
      v236 = v219 - v231
      v237 = v218 - v230
      v238 = v217 - v229
      v239 = v216 - v228
      v240 = v207 * sqrt2
      v241 = v211 + v207
      v242 = v211 * sqrt2
      v243 = (v242 + v211) * 0.5411961
      v244 = (v242 - v211) * 1.306563
      v245 = v240 + v243
      v246 = v240 + v244
      v247 = v240 - v244
      v248 = v240 - v243
      v249 = v241 * sqrt2
      v250 = v241 * sqrt2
      v251 = (v250 + v241) * 0.5411961
      v252 = (v250 - v241) * 1.306563
      v253 = v249 + v251
      v254 = v249 + v252
      v255 = v249 - v252
      v256 = v249 - v251
      v257 = v253 * 0.5097955791041592
      v258 = v254 * 0.6013448869350453
      v259 = v255 * 0.8999762231364156
      v260 = v256 * 2.5629154477415055
      v261 = v245 + v257
      v262 = v246 + v258
      v263 = v247 + v259
      v264 = v248 + v260
      v265 = v248 - v260
      v266 = v247 - v259
      v267 = v246 - v258
      v268 = v245 - v257
      v269 = v261 * 0.5024192861881557
      v270 = v262 * 0.5224986149396889
      v271 = v263 * 0.5669440348163577
      v272 = v264 * 0.6468217833599901
      v273 = v265 * 0.7881546234512502
      v274 = v266 * 1.060677685990347
      v275 = v267 * 1.7224470982383342
      v276 = v268 * 5.101148618689155
      v277 = v232 + v269
      v278 = v233 + v270
      v279 = v234 + v271
      v280 = v235 + v272
      v281 = v236 + v273
      v282 = v237 + v274
      v283 = v238 + v275
      v284 = v239 + v276
      v285 = v239 - v276
      v286 = v238 - v275
      v287 = v237 - v274
      v288 = v236 - v273
      v289 = v235 - v272
      v290 = v234 - v271
      v291 = v233 - v270
      v292 = v232 - v269
      v293 = v206 * sqrt2
      v294 = v208 + v206
      v295 = v210 + v208
      v296 = v212 + v210
      v297 = v212 * sqrt2
      v298 = (v297 + v212) * 0.5411961
      v299 = (v297 - v212) * 1.306563
      v300 = v293 + v298
      v301 = v293 + v299
      v302 = v293 - v299
      v303 = v293 - v298
      v304 = v295 * sqrt2
      v305 = v295 * sqrt2
      v306 = (v305 + v295) * 0.5411961
      v307 = (v305 - v295) * 1.306563
      v308 = v304 + v306
      v309 = v304 + v307
      v310 = v304 - v307
      v311 = v304 - v306
      v312 = v308 * 0.5097955791041592
      v313 = v309 * 0.6013448869350453
      v314 = v310 * 0.8999762231364156
      v315 = v311 * 2.5629154477415055
      v316 = v300 + v312
      v317 = v301 + v313
      v318 = v302 + v314
      v319 = v303 + v315
      v320 = v303 - v315
      v321 = v302 - v314
      v322 = v301 - v313
      v323 = v300 - v312
      v324 = v294 * sqrt2
      v325 = v296 + v294
      v326 = v296 * sqrt2
      v327 = (v326 + v296) * 0.5411961
      v328 = (v326 - v296) * 1.306563
      v329 = v324 + v327
      v330 = v324 + v328
      v331 = v324 - v328
      v332 = v324 - v327
      v333 = v325 * sqrt2
      v334 = v325 * sqrt2
      v335 = (v334 + v325) * 0.5411961
      v336 = (v334 - v325) * 1.306563
      v337 = v333 + v335
      v338 = v333 + v336
      v339 = v333 - v336
      v340 = v333 - v335
      v341 = v337 * 0.5097955791041592
      v342 = v338 * 0.6013448869350453
      v343 = v339 * 0.8999762231364156
      v344 = v340 * 2.5629154477415055
      v345 = v329 + v341
      v346 = v330 + v342
      v347 = v331 + v343
      v348 = v332 + v344
      v349 = v332 - v344
      v350 = v331 - v343
      v351 = v330 - v342
      v352 = v329 - v341
      v353 = v345 * 0.5024192861881557
      v354 = v346 * 0.5224986149396889
      v355 = v347 * 0.5669440348163577
      v356 = v348 * 0.6468217833599901
      v357 = v349 * 0.7881546234512502
      v358 = v350 * 1.060677685990347
      v359 = v351 * 1.7224470982383342
      v360 = v352 * 5.101148618689155
      v361 = v316 + v353
      v362 = v317 + v354
      v363 = v318 + v355
      v364 = v319 + v356
      v365 = v320 + v357
      v366 = v321 + v358
      v367 = v322 + v359
      v368 = v323 + v360
      v369 = v323 - v360
      v370 = v322 - v359
      v371 = v321 - v358
      v372 = v320 - v357
      v373 = v319 - v356
      v374 = v318 - v355
      v375 = v317 - v354
      v376 = v316 - v353
      v377 = v361 * 0.5006029982351963
      v378 = v362 * 0.5054709598975436
      v379 = v363 * 0.5154473099226246
      v380 = v364 * 0.5310425910897841
      v381 = v365 * 0.5531038960344445
      v382 = v366 * 0.5829349682061339
      v383 = v367 * 0.6225041230356648
      v384 = v368 * 0.6748083414550057
      v385 = v369 * 0.7445362710022986
      v386 = v370 * 0.8393496454155268
      v387 = v371 * 0.9725682378619608
      v388 = v372 * 1.1694399334328847
      v389 = v373 * 1.4841646163141662
      v390 = v374 * 2.057781009953411
      v391 = v375 * 3.407608418468719
      v392 = v376 * 10.190008123548033
      v393 = v277 + v377
      v394 = v278 + v378
      v395 = v279 + v379
      v396 = v280 + v380
      v397 = v281 + v381
      v398 = v282 + v382
      v399 = v283 + v383
      v400 = v284 + v384
      v401 = v285 + v385
      v402 = v286 + v386
      v403 = v287 + v387
      v404 = v288 + v388
      v405 = v289 + v389
      v406 = v290 + v390
      v407 = v291 + v391
      v408 = v292 + v392
      v409 = v292 - v392
      v410 = v291 - v391
      v411 = v290 - v390
      v412 = v289 - v389
      v413 = v288 - v388
      v414 = v287 - v387
      v415 = v286 - v386
      v416 = v285 - v385
      v417 = v284 - v384
      v418 = v283 - v383
      v419 = v282 - v382
      v420 = v281 - v381
      v421 = v280 - v380
      v422 = v279 - v379
      v423 = v278 - v378
      v424 = v277 - v377
      v425 = v393 * 0.500150636020651
      v426 = v394 * 0.5013584524464084
      v427 = v395 * 0.5037887256810443
      v428 = v396 * 0.5074711720725553
      v429 = v397 * 0.5124514794082247
      v430 = v398 * 0.5187927131053328
      v431 = v399 * 0.52657731515427
      v432 = v400 * 0.535909816907992
      v433 = v401 * 0.5469204379855088
      v434 = v402 * 0.5597698129470802
      v435 = v403 * 0.57465518403266
      v436 = v404 * 0.5918185358574165
      v437 = v405 * 0.6115573478825099
      v438 = v406 * 0.6342389366884031
      v439 = v407 * 0.6603198078137061
      v440 = v408 * 0.6903721282002123
      v441 = v409 * 0.7251205223771985
      v442 = v410 * 0.7654941649730891
      v443 = v411 * 0.8127020908144905
      v444 = v412 * 0.8683447152233481
      v445 = v413 * 0.9345835970364075
      v446 = v414 * 1.0144082649970547
      v447 = v415 * 1.1120716205797176
      v448 = v416 * 1.233832737976571
      v449 = v417 * 1.3892939586328277
      v450 = v418 * 1.5939722833856311
      v451 = v419 * 1.8746759800084078
      v452 = v420 * 2.282050068005162
      v453 = v421 * 2.924628428158216
      v454 = v422 * 4.084611078129248
      v455 = v423 * 6.796750711673633
      v456 = v424 * 20.373878167231453
      v457 = v173 + v425
      v458 = v174 + v426
      v459 = v175 + v427
      v460 = v176 + v428
      v461 = v177 + v429
      v462 = v178 + v430
      v463 = v179 + v431
      v464 = v180 + v432
      v465 = v181 + v433
      v466 = v182 + v434
      v467 = v183 + v435
      v468 = v184 + v436
      v469 = v185 + v437
      v470 = v186 + v438
      v471 = v187 + v439
      v472 = v188 + v440
      v473 = v189 + v441
      v474 = v190 + v442
      v475 = v191 + v443
      v476 = v192 + v444
      v477 = v193 + v445
      v478 = v194 + v446
      v479 = v195 + v447
      v480 = v196 + v448
      v481 = v197 + v449
      v482 = v198 + v450
      v483 = v199 + v451
      v484 = v200 + v452
      v485 = v201 + v453
      v486 = v202 + v454
      v487 = v203 + v455
      v488 = v204 + v456
      v489 = v204 - v456
      v490 = v203 - v455
      v491 = v202 - v454
      v492 = v201 - v453
      v493 = v200 - v452
      v494 = v199 - v451
      v495 = v198 - v450
      v496 = v197 - v449
      v497 = v196 - v448
      v498 = v195 - v447
      v499 = v194 - v446
      v500 = v193 - v445
      v501 = v192 - v444
      v502 = v191 - v443
      v503 = v190 - v442
      v504 = v189 - v441
      v505 = v188 - v440
      v506 = v187 - v439
      v507 = v186 - v438
      v508 = v185 - v437
      v509 = v184 - v436
      v510 = v183 - v435
      v511 = v182 - v434
      v512 = v181 - v433
      v513 = v180 - v432
      v514 = v179 - v431
      v515 = v178 - v430
      v516 = v177 - v429
      v517 = v176 - v428
      v518 = v175 - v427
      v519 = v174 - v426
      v520 = v173 - v425
  wr a (off + 0) v457
  wr a (off + 1) v458
  wr a (off + 2) v459
  wr a (off + 3) v460
  wr a (off + 4) v461
  wr a (off + 5) v462
  wr a (off + 6) v463
  wr a (off + 7) v464
  wr a (off + 8) v465
  wr a (off + 9) v466
  wr a (off + 10) v467
  wr a (off + 11) v468
  wr a (off + 12) v469
  wr a (off + 13) v470
  wr a (off + 14) v471
  wr a (off + 15) v472
  wr a (off + 16) v473
  wr a (off + 17) v474
  wr a (off + 18) v475
  wr a (off + 19) v476
  wr a (off + 20) v477
  wr a (off + 21) v478
  wr a (off + 22) v479
  wr a (off + 23) v480
  wr a (off + 24) v481
  wr a (off + 25) v482
  wr a (off + 26) v483
  wr a (off + 27) v484
  wr a (off + 28) v485
  wr a (off + 29) v486
  wr a (off + 30) v487
  wr a (off + 31) v488
  wr a (off + 32) v489
  wr a (off + 33) v490
  wr a (off + 34) v491
  wr a (off + 35) v492
  wr a (off + 36) v493
  wr a (off + 37) v494
  wr a (off + 38) v495
  wr a (off + 39) v496
  wr a (off + 40) v497
  wr a (off + 41) v498
  wr a (off + 42) v499
  wr a (off + 43) v500
  wr a (off + 44) v501
  wr a (off + 45) v502
  wr a (off + 46) v503
  wr a (off + 47) v504
  wr a (off + 48) v505
  wr a (off + 49) v506
  wr a (off + 50) v507
  wr a (off + 51) v508
  wr a (off + 52) v509
  wr a (off + 53) v510
  wr a (off + 54) v511
  wr a (off + 55) v512
  wr a (off + 56) v513
  wr a (off + 57) v514
  wr a (off + 58) v515
  wr a (off + 59) v516
  wr a (off + 60) v517
  wr a (off + 61) v518
  wr a (off + 62) v519
  wr a (off + 63) v520

-- Forward 1-D transform (only the LLF coefficients of large blocks need it).
fdct1 :: FPlane s -> Int -> Int -> FPlane s -> Int -> ST s ()
fdct1 a off n t toff
  | n <= 1 = pure ()
  | n == 2 = do
      x0 <- rd a off; x1 <- rd a (off + 1)
      wr a off ((x0 + x1) / 2); wr a (off + 1) ((x0 - x1) / 2)
  | n == 4 = do
      i0 <- rd a off; i1 <- rd a (off + 1); i2 <- rd a (off + 2); i3 <- rd a (off + 3)
      let s03 = i0 + i3
          s12 = i1 + i2
          t0 = (i0 - i3) * 0.5411961
          t1 = (i1 - i2) * 1.306563
          o0 = (t0 + t1) / 4
          o1 = (t0 - t1) / 4
      wr a off ((s03 + s12) / 4); wr a (off + 1) (o0 * sqrt2 + o1)
      wr a (off + 2) ((s03 - s12) / 4); wr a (off + 3) o1
  | otherwise = do
      let h = n `shiftR` 1
      forLoop 0 (h - 1) $ \i -> do
        x <- rd a (off + i)
        y <- rd a (off + n - 1 - i)
        wr t (toff + i) ((x + y) / 2)
        wr t (toff + h + i) ((x - y) / 2 * (secTab `unsafeAt` (h + i)))
      fdct1 t toff h a off
      fdct1 t (toff + h) h a (off + h)
      v0 <- rd t (toff + h)
      wr t (toff + h) (v0 * sqrt2)
      forLoop 0 (h - 2) $ \i -> do
        v <- rd t (toff + h + i)
        u <- rd t (toff + h + i + 1)
        wr t (toff + h + i) (v + u)
      forLoop 0 (h - 1) $ \i -> do
        rd t (toff + i) >>= wr a (off + 2 * i)
        rd t (toff + h + i) >>= wr a (off + 2 * i + 1)

-- 2-D transform of a w x h block at arr[off] with the given row stride:
-- rows, then columns, gathered sixteen at a time (one cache line of each
-- row) into contiguous buffers: a plane's stride is a power of two, so
-- walking one column at a time maps every row to the same cache set.
xform2d :: Bool -> Scratch s -> FPlane s -> Int -> Int -> Int -> Int -> ST s ()
xform2d inverse (Scratch _ t col) arr off stride w h = do
  let f = if inverse then idct1 else fdct1
  -- an all-zero row transforms to zeros, and the high-frequency rows of a
  -- large block usually are
  lastRow <- if w <= 1 then pure h else do
    let rows !y !lastNZ
          | y >= h = pure lastNZ
          | otherwise = do
              let ro = off + y * stride
                  nz !i | i < 0 = pure 0
                        | otherwise = do
                            v <- rd arr (ro + i)
                            if v /= 0 then pure (i + 1) else nz (i - 1)
              live <- if inverse then nz (w - 1) else pure w
              when (live > 0) $
                if inverse then idctPruned arr ro w live t 0 else fdct1 arr ro w t 0
              rows (y + 1) (if live > 0 then y + 1 else lastNZ)
    rows 0 0
  -- Match the selected kernel's actual input extent. Generated kernels
  -- read their whole 4/8/16 prefix, including its proven zero padding;
  -- full-kernel fallbacks must still gather every row. Force this once
  -- outside the chunk loop: a lazy bound here otherwise allocates per block.
  let !extent
        | not inverse || h < 16 || lastRow > h `div` 2 = h
        | lastRow <= 1 = 1
        | h == 16 && lastRow <= 4 = 4
        | h == 32 && lastRow <= 4 = 4
        | h == 32 && lastRow <= 8 = 8
        | h == 64 && lastRow <= 8 = 8
        | h == 64 && lastRow <= 16 = 16
        | h <= 32 = h
        | otherwise = lastRow
  when (h > 1) $ forLoopStep 0 (w - 1) 16 $ \x0 -> do
    let nc = min 16 (w - x0)
    forLoop 0 (extent - 1) $ \y -> do
      let src = off + y * stride + x0
      forLoop 0 (nc - 1) $ \k -> rd arr (src + k) >>= wr col (k * h + y)
    forLoop 0 (nc - 1) $ \k ->
      if inverse && lastRow <= h `div` 2 && h >= 16
        then idctPruned col (k * h) h lastRow t 256
        else f col (k * h) h t 256
    forLoop 0 (h - 1) $ \y -> do
      let dst = off + y * stride + x0
      forLoop 0 (nc - 1) $ \k -> rd col (k * h + y) >>= wr arr (dst + k)

------------------------------------------------------------------------------
-- Varblock transforms (§I.6)

scaleF :: UArray Int Float
scaleF = listArray (0, 31)
  [ 1.0000000000000000, 0.9996047255830407, 0.9984194528776054, 0.9964458326264695
  , 0.9936866130906366, 0.9901456355893141, 0.9858278282666936, 0.9807391980963174
  , 0.9748868211368796, 0.9682788310563117, 0.9609244059440204, 0.9528337534340876
  , 0.9440180941651672, 0.9344896436056892, 0.9242615922757944, 0.9133480844001980
  , 0.9017641950288744, 0.8895259056651056, 0.8766500784429904, 0.8631544288990163
  , 0.8490574973847023, 0.8343786191696513, 0.8191378932865928, 0.8033561501721485
  , 0.7870549181591013, 0.7702563888779096, 0.7529833816270532, 0.7352593067735488
  , 0.7171081282466044, 0.6985543251889097, 0.6796228528314652, 0.6603391026591464 ]

-- | The LLF coefficients of a varblock larger than 8x8, from its bw8 x bh8
-- LF values (read from @lf@ at @lfOff@ with stride @lfStride@), written into
-- the top-left of the block at plane offset @off@.
llfFromLf :: Scratch s -> FPlane s -> Int -> Int -> FPlane s -> Int -> Int -> Int -> Int -> ST s ()
llfFromLf sc@(Scratch big _ _) lf lfOff lfStride plane off stride bw8 bh8 = do
  -- big[0 ..] is free working space here
  forLoop 0 (bh8 - 1) $ \y -> forLoop 0 (bw8 - 1) $ \x ->
    rd lf (lfOff + y * lfStride + x) >>= wr big (y * bw8 + x)
  xform2d False sc big 0 bw8 bw8 bh8
  let lbw = countTrailingZeros bw8
      lbh = countTrailingZeros bh8
  forLoop 0 (bh8 - 1) $ \y -> forLoop 0 (bw8 - 1) $ \x -> do
    v <- rd big (y * bw8 + x)
    let s = (scaleF `unsafeAt` (y `shiftL` (5 - lbh))) * (scaleF `unsafeAt` (x `shiftL` (5 - lbw)))
    wr plane (off + y * stride + x) (v / s)

-- | Inverse-transform one varblock of type @t@ in place at plane offset
-- @off@ (row stride @stride@).
idctBlock :: Scratch s -> Int -> FPlane s -> Int -> Int -> ST s ()
idctBlock sc t plane off stride = case t of
  2 -> do aux 2; aux 4; aux 8
  3 -> dct4
  1 -> hornuss
  12 -> dct4x8 False
  13 -> dct4x8 True
  14 -> afv 0
  15 -> afv 1
  16 -> afv 2
  17 -> afv 3
  _ -> xform2d True sc plane off stride (8 * dctW8 `unsafeAt` t) (8 * dctH8 `unsafeAt` t)
  where
    Scratch big _ small = sc
    ix x y = off + y * stride + x
    get x y = rd plane (ix x y)
    put x y v = wr plane (ix x y) v
    -- AuxIDCT2x2 over the top-left size x size corner
    aux size = do
      let num = size `shiftR` 1
      forLoop 0 (num - 1) $ \y -> forLoop 0 (num - 1) $ \x -> do
        c00 <- get x y
        c01 <- get (x + num) y
        c10 <- get x (y + num)
        c11 <- get (x + num) (y + num)
        wr small ((2 * y) * 8 + 2 * x) (c00 + c01 + c10 + c11)
        wr small ((2 * y) * 8 + 2 * x + 1) (c00 + c01 - c10 - c11)
        wr small ((2 * y + 1) * 8 + 2 * x) (c00 - c01 + c10 - c11)
        wr small ((2 * y + 1) * 8 + 2 * x + 1) (c00 - c01 - c10 + c11)
      forLoop 0 (size - 1) $ \y -> forLoop 0 (size - 1) $ \x ->
        rd small (y * 8 + x) >>= put x y
    dct4 = do
      aux 2
      forLoop 0 1 $ \y -> forLoop 0 1 $ \x -> do
        let b = (y * 2 + x) * 16
        forLoop 0 3 $ \iy -> forLoop 0 3 $ \ix ->
          get (x + ix * 2) (y + iy * 2) >>= wr big (b + ix * 4 + iy)
        xform2d True sc big b 4 4 4
      forLoop 0 1 $ \y -> forLoop 0 1 $ \x -> do
        let b = (y * 2 + x) * 16
        forLoop 0 3 $ \iy -> forLoop 0 3 $ \ix ->
          rd big (b + iy * 4 + ix) >>= put (x * 4 + ix) (y * 4 + iy)
    hornuss = do
      aux 2
      forLoop 0 1 $ \y -> forLoop 0 1 $ \x -> do
        let b = (y * 2 + x) * 16
        forLoop 0 3 $ \iy -> forLoop 0 3 $ \ix ->
          get (x + ix * 2) (y + iy * 2) >>= wr big (b + iy * 4 + ix)
        vs <- mapM (\i -> rd big (b + i)) [1 .. 15]
        s0 <- rd big b
        s5 <- rd big (b + 5)
        let avg = s0 - sum vs / 16
        wr big b s5
        wr big (b + 5) 0
        forLoop 0 15 $ \i -> do v <- rd big (b + i); wr big (b + i) (v + avg)
      forLoop 0 1 $ \y -> forLoop 0 1 $ \x -> do
        let b = (y * 2 + x) * 16
        forLoop 0 3 $ \iy -> forLoop 0 3 $ \ix ->
          rd big (b + iy * 4 + ix) >>= put (x * 4 + ix) (y * 4 + iy)
    dct4x8 tr = do
      c0 <- get 0 0
      c1 <- get 0 1
      put 0 0 (c0 + c1)
      put 0 1 (c0 - c1)
      forLoop 0 1 $ \idx -> do
        let b = idx * 32
        forLoop 0 3 $ \iy -> forLoop 0 7 $ \ix ->
          get ix (iy * 2 + idx) >>= wr big (b + iy * 8 + ix)
        xform2d True sc big b 8 8 4
      forLoop 0 7 $ \y -> forLoop 0 7 $ \x -> do
        v <- rd big (y * 8 + x)
        if tr then put y x v else put x y v
    afv n = do
      let flipX = n .&. 1
          flipY = n `shiftR` 1
      c00 <- get 0 0
      c10 <- get 1 0
      c01 <- get 0 1
      -- AFV coefficients -> big[0 .. 16), samples -> big[16 .. 32)
      wr big 0 ((c00 + c10 + c01) * 4)
      forLoop 1 15 $ \idx -> get (2 * (idx .&. 3)) (2 * (idx `shiftR` 2)) >>= wr big idx
      forLoop 0 15 $ \j -> do
        let go !i !acc | i > 15 = pure acc
                       | otherwise = do
                           cv <- rd big i
                           go (i + 1) (acc + cv * (afvBasis `unsafeAt` (i * 16 + j)))
        s <- go 0 0
        wr big (16 + j) s
      -- 4x4 DCT -> big[32 .. 48), 4x8 DCT -> big[48 .. 80)
      wr big 32 (c00 - c10 + c01)
      forLoop 0 3 $ \iy -> forLoop 0 3 $ \ix ->
        unless (ix == 0 && iy == 0) $ get (2 * ix + 1) (2 * iy) >>= wr big (32 + ix * 4 + iy)
      xform2d True sc big 32 4 4 4
      wr big 48 (c00 - c01)
      forLoop 0 3 $ \iy -> forLoop 0 7 $ \ix ->
        unless (ix == 0 && iy == 0) $ get ix (2 * iy + 1) >>= wr big (48 + iy * 8 + ix)
      xform2d True sc big 48 8 8 4
      forLoop 0 3 $ \iy -> forLoop 0 3 $ \ix -> do
        let ay = if flipY == 0 then iy else 3 - iy
            ax = if flipX == 0 then ix else 3 - ix
        rd big (16 + ay * 4 + ax) >>= put (flipX * 4 + ix) (flipY * 4 + iy)
      forLoop 0 3 $ \iy -> forLoop 0 3 $ \ix ->
        rd big (32 + iy * 4 + ix) >>= put ((1 - flipX) * 4 + ix) (flipY * 4 + iy)
      forLoop 0 3 $ \iy -> forLoop 0 7 $ \ix ->
        rd big (48 + iy * 8 + ix) >>= put ix ((1 - flipY) * 4 + iy)

afvBasis :: UArray Int Float
afvBasis = listArray (0, 255)
  [ 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25
  , 0.876902929799142, 0.2206518106944235, -0.10140050393753763, -0.1014005039375375
  , 0.2206518106944236, -0.10140050393753777, -0.10140050393753772, -0.10140050393753763
  , -0.10140050393753758, -0.10140050393753769, -0.1014005039375375, -0.10140050393753768
  , -0.10140050393753768, -0.10140050393753759, -0.10140050393753763, -0.10140050393753741
  , 0.0, 0.0, 0.40670075830260755, 0.44444816619734445
  , 0.0, 0.0, 0.19574399372042936, 0.2929100136981264
  , -0.40670075830260716, -0.19574399372042872, 0.0, 0.11379074460448091
  , -0.44444816619734384, -0.29291001369812636, -0.1137907446044814, 0.0
  , 0.0, 0.0, -0.21255748058288748, 0.3085497062849767
  , 0.0, 0.4706702258572536, -0.1621205195722993, 0.0
  , -0.21255748058287047, -0.16212051957228327, -0.47067022585725277, -0.1464291867126764
  , 0.3085497062849487, 0.0, -0.14642918671266536, 0.4251149611657548
  , 0.0, -0.7071067811865474, 0.0, 0.0
  , 0.7071067811865476, 0.0, 0.0, 0.0
  , 0.0, 0.0, 0.0, 0.0
  , 0.0, 0.0, 0.0, 0.0
  , -0.4105377591765233, 0.6235485373547691, -0.06435071657946274, -0.06435071657946266
  , 0.6235485373547694, -0.06435071657946284, -0.0643507165794628, -0.06435071657946274
  , -0.06435071657946272, -0.06435071657946279, -0.06435071657946266, -0.06435071657946277
  , -0.06435071657946277, -0.06435071657946273, -0.06435071657946274, -0.0643507165794626
  , 0.0, 0.0, -0.4517556589999482, 0.15854503551840063
  , 0.0, -0.04038515160822202, 0.0074182263792423875, 0.39351034269210167
  , -0.45175565899994635, 0.007418226379244351, 0.1107416575309343, 0.08298163094882051
  , 0.15854503551839705, 0.3935103426921022, 0.0829816309488214, -0.45175565899994796
  , 0.0, 0.0, -0.304684750724869, 0.5112616136591823
  , 0.0, 0.0, -0.290480129728998, -0.06578701549142804
  , 0.304684750724884, 0.2904801297290076, 0.0, -0.23889773523344604
  , -0.5112616136592012, 0.06578701549142545, 0.23889773523345467, 0.0
  , 0.0, 0.0, 0.3017929516615495, 0.25792362796341184
  , 0.0, 0.16272340142866204, 0.09520022653475037, 0.0
  , 0.3017929516615503, 0.09520022653475055, -0.16272340142866173, -0.35312385449816297
  , 0.25792362796341295, 0.0, -0.3531238544981624, -0.6035859033230976
  , 0.0, 0.0, 0.40824829046386274, 0.0
  , 0.0, 0.0, 0.0, -0.4082482904638628
  , -0.4082482904638635, 0.0, 0.0, -0.40824829046386296
  , 0.0, 0.4082482904638634, 0.408248290463863, 0.0
  , 0.0, 0.0, 0.1747866975480809, 0.0812611176717539
  , 0.0, 0.0, -0.3675398009862027, -0.307882213957909
  , -0.17478669754808135, 0.3675398009862011, 0.0, 0.4826689115059883
  , -0.08126111767175039, 0.30788221395790305, -0.48266891150598584, 0.0
  , 0.0, 0.0, -0.21105601049335784, 0.18567180916109802
  , 0.0, 0.0, 0.49215859013738733, -0.38525013709251915
  , 0.21105601049335806, -0.49215859013738905, 0.0, 0.17419412659916217
  , -0.18567180916109904, 0.3852501370925211, -0.1741941265991621, 0.0
  , 0.0, 0.0, -0.14266084808807264, -0.3416446842253372
  , 0.0, 0.7367497537172237, 0.24627107722075148, -0.08574019035519306
  , -0.14266084808807344, 0.24627107722075137, 0.14883399227113567, -0.04768680350229251
  , -0.3416446842253373, -0.08574019035519267, -0.047686803502292804, -0.14266084808807242
  , 0.0, 0.0, -0.13813540350758585, 0.3302282550303788
  , 0.0, 0.08755115000587084, -0.07946706605909573, -0.4613374887461511
  , -0.13813540350758294, -0.07946706605910261, 0.49724647109535086, 0.12538059448563663
  , 0.3302282550303805, -0.4613374887461554, 0.12538059448564315, -0.13813540350758452
  , 0.0, 0.0, -0.17437602599651067, 0.0702790691196284
  , 0.0, -0.2921026642334881, 0.3623817333531167, 0.0
  , -0.1743760259965108, 0.36238173335311646, 0.29210266423348785, -0.4326608024727445
  , 0.07027906911962818, 0.0, -0.4326608024727457, 0.34875205199302267
  , 0.0, 0.0, 0.11354987314994337, -0.07417504595810355
  , 0.0, 0.19402893032594343, -0.435190496523228, 0.21918684838857466
  , 0.11354987314994257, -0.4351904965232251, 0.5550443808910661, -0.25468277124066463
  , -0.07417504595810233, 0.2191868483885728, -0.25468277124066413, 0.1135498731499429 ]

------------------------------------------------------------------------------
-- Dequantisation matrices (§I.2.4)

-- | One parameter set's encoding, per channel where it has channels.
data DqParams
  = DqDct [[Float]]                          -- band parameters
  | DqHornuss [[Float]]
  | DqDct2 [[Float]]
  | DqDct4 [[Float]] [[Float]]               -- params, dct params
  | DqDct4x8 [[Float]] [[Float]]
  | DqAfv [[Float]] [[Float]] [[Float]]      -- params, dct params, 4x4 params
  | DqRaw Float [[Int]]                      -- denominator, channel samples

defaultDqParams :: Int -> DqParams
defaultDqParams p = case p of
  0 -> DqDct [ [3150, 0, -0.4, -0.4, -0.4, -2], [560, 0, -0.3, -0.3, -0.3, -0.3], [512, -2, -1, 0, -1, -2] ]
  1 -> DqHornuss [ [280, 3160, 3160], [60, 864, 864], [18, 200, 200] ]
  2 -> DqDct2 [ [3840, 2560, 1280, 640, 480, 300], [960, 640, 320, 180, 140, 120], [640, 320, 128, 64, 32, 16] ]
  3 -> DqDct4 (replicate 3 [1, 1]) dct4Params
  4 -> DqDct [ [8996.873, -1.3000778, -0.4942453, -0.43909377, -0.6350102, -0.9017726, -1.6162099]
             , [3191.4836, -0.67424583, -0.80745816, -0.4492584, -0.3586544, -0.3132239, -0.37615025]
             , [1157.504, -2.0531423, -1.4, -0.5068713, -0.4270873, -1.4856834, -4.920914] ]
  5 -> DqDct [ [15718.408, -1.025, -0.98, -0.9012, -0.4, -0.48819396, -0.421064, -0.27]
             , [7305.7637, -0.8041958, -0.76330364, -0.5566038, -0.49785304, -0.43699592, -0.40180868, -0.27321684]
             , [3803.5317, -3.0607336, -2.041327, -2.023565, -0.54953897, -0.4, -0.4, -0.3] ]
  6 -> DqDct [ [7240.7734, -0.7, -0.7, -0.2, -0.2, -0.2, -0.5]
             , [1448.1547, -0.5, -0.5, -0.5, -0.2, -0.2, -0.2]
             , [506.85413, -1.4, -0.2, -0.5, -0.5, -1.5, -3.6] ]
  7 -> DqDct [ [16283.249, -1.7812846, -1.6309059, -1.0382179, -0.85, -0.7, -0.9, -1.2360638]
             , [5089.1577, -0.3200494, -0.3536285, -0.3034, -0.61, -0.5, -0.5, -0.6]
             , [3397.7761, -0.32132736, -0.3450762, -0.7034, -0.9, -1.0, -1.0, -1.1754606] ]
  8 -> DqDct [ [13844.971, -0.971138, -0.658, -0.42026, -0.22712, -0.2206, -0.226, -0.6]
             , [4798.964, -0.6112531, -0.8377079, -0.7901486, -0.26927274, -0.38272768, -0.22924222, -0.20719099]
             , [1807.2369, -1.2, -1.2, -0.7, -0.7, -0.7, -0.4, -0.5] ]
  9 -> DqDct4x8 (replicate 3 [1]) dct4x8Params
  10 -> DqAfv [ [3072, 3072, 256, 256, 256, 414, 0, 0, 0]
              , [1024, 1024, 50, 50, 50, 58, 0, 0, 0]
              , [384, 384, 12, 12, 12, 22, -0.25, -0.25, -0.25] ] dct4x8Params dct4Params
  11 -> common 23966.166 8380.191 4493.024
  12 -> common 15358.898 5597.3604 2919.9617
  13 -> common 47932.332 16760.383 8986.048
  14 -> common 30717.797 11194.721 5839.9233
  15 -> common 95864.664 33520.766 17972.096
  _ -> common 61435.594 24209.441 12979.847
  where
    common a b c = DqDct [ a : [-1.025, -0.78, -0.65012, -0.19041574, -0.20819396, -0.421064, -0.32733846]
                         , b : [-0.30419582, -0.36330363, -0.3566038, -0.34430745, -0.33699593, -0.30180866, -0.27321684]
                         , c : [-1.2, -1.2, -0.8, -0.7, -0.7, -0.4, -0.5] ]

dct4Params, dct4x8Params :: [[Float]]
dct4Params = [ [2200, 0, 0, 0], [392, 0, 0, 0], [112, -0.25, -0.25, -0.5] ]
dct4x8Params = [ [2198.0505, -0.96269625, -0.7619425, -0.65511405]
               , [764.36554, -0.926302, -0.967523, -0.2784529]
               , [527.10754, -1.4594386, -1.4500821, -1.5843723] ]

-- | Read one parameter set (mode 7, raw, is decoded by the caller through
-- the supplied modular reader).
readDqParams :: BR s -> Int -> (Int -> Int -> J s [[Int]]) -> J s DqParams
readDqParams br p readRaw = do
  mode <- liftJ (readBits br 3)
  jguard (mode == 0 || mode >= 6 || p `elem` [0, 1, 2, 3, 9, 10]) "JPEG XL: bad dequant matrix mode"
  let f16s n = replicateM n (f16' br)
      fixed n = replicateM 3 (f16s n)
      dctPs = do
        n <- liftJ ((+ 1) <$> readBits br 4)
        replicateM 3 $ do
          vs <- f16s n
          pure (head vs * 64 : tail vs)
  case mode of
    0 -> pure (defaultDqParams p)
    1 -> DqHornuss <$> fixed 3
    2 -> DqDct2 <$> fixed 6
    3 -> DqDct4 <$> fixed 2 <*> dctPs
    4 -> DqDct4x8 <$> fixed 1 <*> dctPs
    5 -> do
      ps <- fixed 9
      let ps' = [ map (* 64) (take 6 c) ++ drop 6 c | c <- ps ]
      DqAfv ps' <$> dctPs <*> dctPs
    6 -> DqDct <$> dctPs
    _ -> do
      den <- f16' br
      chans <- readRaw (matrixW `unsafeAt` p) (matrixH `unsafeAt` p)
      pure (DqRaw den chans)
  where
    f16' b = liftJ (f16 b) >>= maybe (jfail "JPEG XL: invalid float") (pure . realToFrac)

-- | The three per-channel multiplier matrices (1 / weight, row-major in the
-- wide orientation) for parameter set @p@.
buildMatrix :: Int -> DqParams -> Either String [UArray Int Float]
buildMatrix p enc = do
  ws <- case enc of
    DqDct bands -> mapM (\b -> weights b mw mh) bands
    DqHornuss ps -> pure [ hornuss c | c <- ps ]
    DqDct2 ps -> pure [ dct2 c | c <- ps ]
    DqDct4 ps dps -> sequence [ dct4w c d | (c, d) <- zip ps dps ]
    DqDct4x8 ps dps -> sequence [ dct4x8w c d | (c, d) <- zip ps dps ]
    DqAfv ps dps d4 -> sequence [ afvw c d e | (c, d, e) <- zip3 ps dps d4 ]
    DqRaw den chans -> pure [ [ fromIntegral v * den | v <- c ] | c <- chans ]
  let recip' = case enc of { DqRaw _ _ -> False; _ -> True }
      ws' = if recip' then map (map (1 /)) ws else ws
  if or [ v >= 1e8 || v <= 0 || isNaN v | c <- ws', v <- c ]
    then Left "JPEG XL: bad dequant matrix"
    else Right [ listArray (0, mw * mh - 1) c | c <- ws' ]
  where
    mw = matrixW `unsafeAt` p
    mh = matrixH `unsafeAt` p
    mult x = if x > 0 then 1 + x else 1 / (1 - x)
    bandsOf (b0 : rest) = scanl (\l v -> l * mult v) b0 rest
    bandsOf [] = []
    interp pos maxv bands = case bands of
      [v] -> v
      _ -> let len = length bands
               sp = pos * fromIntegral (len - 1) / maxv
               si = truncate sp :: Int
               fr = sp - fromIntegral si
               a = bands !! si
               b = bands !! (si + 1)
           in a * (b / a) ** fr
    weights ps w h = do
      let bs = bandsOf ps
      if any (<= 0) bs then Left "JPEG XL: bad dequant bands" else Right
        [ interp (sqrt (dx * dx + dy * dy)) (sqrt 2 + 1e-6) bs
        | y <- [0 .. h - 1], x <- [0 .. w - 1]
        , let dx = fromIntegral x / fromIntegral (w - 1)
              dy = fromIntegral y / fromIntegral (h - 1) ]
    hornuss [a, b, c] = [ if i == 0 then 1 else if i == 1 || i == 8 then b else if i == 9 then c else a
                        | i <- [0 .. 63 :: Int] ]
    hornuss _ = replicate 64 1
    dct2 ps = [ dct2At (i `mod` 8) (i `div` 8) | i <- [0 .. 63 :: Int] ]
      where dct2At x y
              | x == 0 && y == 0 = 1
              | otherwise =
                  let m = max x y
                      sh = if m >= 4 then 2 else if m >= 2 then 1 else 0
                      dim = 1 `shiftL` sh
                      inBoth = x >= dim && y >= dim
                  in ps !! (2 * sh + (if inBoth then 1 else 0))
    dct4w ps dps = do
      mat <- weights dps 4 4
      let base = [ mat !! ((y `div` 2) * 4 + x `div` 2) | y <- [0 .. 7 :: Int], x <- [0 .. 7 :: Int] ]
          upd i v | i == 1 || i == 8 = v / head ps
                  | i == 9 = v / (ps !! 1)
                  | otherwise = v
      pure [ upd i v | (i, v) <- zip [0 :: Int ..] base ]
    dct4x8w ps dps = do
      mat <- weights dps 8 4
      let base = [ mat !! ((y `div` 2) * 8 + x) | y <- [0 .. 7 :: Int], x <- [0 .. 7 :: Int] ]
      pure [ if i == 8 then v / head ps else v | (i, v) <- zip [0 :: Int ..] base ]
    afvw ps dps d4 = do
      w48 <- weights dps 8 4
      w44 <- weights d4 4 4
      let freqs = [0, 0, 0.8517779, 5.3777843, 0, 0, 4.734748, 5.4492455, 1.659827, 4.0
                  , 7.275749, 10.423227, 2.6629324, 7.6306577, 8.962389, 12.971662] :: [Float]
          flo = freqs !! 2
          fhi = freqs !! 15
          bands = scanl (\l v -> l * mult v) (ps !! 5) (take 3 (drop 6 ps))
          at x y
            -- even rows, even columns: the AFV weights
            | even y && even x =
                let ax = x `div` 2; ay = y `div` 2
                in case (ax, ay) of
                     (0, 0) -> 1
                     (0, 1) -> ps !! 2
                     (1, 0) -> ps !! 3
                     (1, 1) -> ps !! 4
                     _ -> interp (freqs !! (ay * 4 + ax) - flo) (fhi - flo + 1e-6) bands
            -- even rows, odd columns: the 4x4 DCT weights
            | even y = let ax = x `div` 2; ay = y `div` 2
                       in if ax == 0 && ay == 0 then ps !! 1 else w44 !! (ay * 4 + ax)
            -- odd rows: the 4x8 DCT weights
            | otherwise = let ay = y `div` 2
                          in if ay == 0 && x == 0 then head ps else w48 !! (ay * 8 + x)
      pure [ at x y | y <- [0 .. 7 :: Int], x <- [0 .. 7 :: Int] ]

------------------------------------------------------------------------------
-- Coefficient orders (§I.3.2)

-- | The natural order of an order id: (x, y) coordinates packed y << 16 | x,
-- in the wide orientation.
naturalOrder :: Int -> UArray Int Int
naturalOrder oid =
  let bw = orderBW `unsafeAt` oid
      bh = orderBH `unsafeAt` oid
      yScale = bw `div` bh
      lbw = bw `div` 8
      lbh = bh `div` 8
      llf = [ (y `shiftL` 16) .|. x | y <- [0 .. lbh - 1], x <- [0 .. lbw - 1] ]
      rest = [ ((y `div` yScale) `shiftL` 16) .|. x
             | dist <- [1 .. 2 * bw - 1]
             , let margin = max 0 (dist - bw)
             , order <- [margin .. dist - margin - 1]
             , let (x, y) = if odd dist then (order, dist - 1 - order) else (dist - 1 - order, order)
             , not (x < lbw && y < lbw)
             , y `mod` yScale == 0 ]
  in listArray (0, bw * bh - 1) (llf ++ rest)

------------------------------------------------------------------------------
-- LF smoothing and the restoration filters

-- | Adaptive LF smoothing (§J.2? applied to the whole LF image, XYB planes
-- of w x h with stride w), with the per-channel inverse quantisation scales.
adaptiveLfSmoothing :: [FPlane s] -> Int -> Int -> [Float] -> ST s ()
adaptiveLfSmoothing planes w h scales = when (w > 2 && h > 2) $ do
  let [px, py, pb] = planes
      [lx, ly, lb] = scales
  -- copies of the unsmoothed rows above, current and below
  bufs <- forM planes $ \_ -> newArray (0, 3 * w - 1) 0 :: ST s (FPlane s)
  let copyRow p b slot y = forLoop 0 (w - 1) $ \x -> rd p (y * w + x) >>= wr b (slot * w + x)
  forM_ (zip planes bufs) $ \(p, b) -> do copyRow p b 0 0; copyRow p b 1 1
  let [bx, by, bb] = bufs
  forLoop 1 (h - 2) $ \y -> do
    let up = (y - 1) `mod` 3
        cu = y `mod` 3
        dn = (y + 1) `mod` 3
    forM_ (zip planes bufs) $ \(p, b) -> copyRow p b dn (y + 1)
    forLoop 1 (w - 2) $ \x -> do
      let wa b = do
            s <- rd b (cu * w + x)
            l <- rd b (cu * w + x - 1)
            r <- rd b (cu * w + x + 1)
            u <- rd b (up * w + x)
            d <- rd b (dn * w + x)
            ul <- rd b (up * w + x - 1)
            ur <- rd b (up * w + x + 1)
            dl <- rd b (dn * w + x - 1)
            dr <- rd b (dn * w + x + 1)
            pure (s, s * 0.052262735 + (l + r + u + d) * 0.2034514 + (ul + ur + dl + dr) * 0.03348292)
      (sx, wx) <- wa bx
      (sy, wy) <- wa by
      (sb, wb) <- wa bb
      let gap = maximum [0.5, abs (wx - sx) / lx, abs (wy - sy) / ly, abs (wb - sb) / lb]
          g = max 0 (3 - 4 * gap)
      wr px (y * w + x) ((wx - sx) * g + sx)
      wr py (y * w + x) ((wy - sy) * g + sy)
      wr pb (y * w + x) ((wb - sb) * g + sb)

-- | Gaborish: a 3x3 convolution per channel with mirrored edges, in place on
-- the w x h region of planes of the given stride.
gaborish :: [FPlane s] -> Int -> Int -> Int -> [Double] -> ST s ()
gaborish planes stride w h ws = when (w > 0 && h > 0) $
  forM_ (zip planes [0 ..]) $ \(p, c) -> do
    let w1 = realToFrac (ws !! (2 * c)) :: Float
        w2 = realToFrac (ws !! (2 * c + 1)) :: Float
        g = 1 / (1 + 4 * w1 + 4 * w2)
    rows <- newArray (0, 3 * w - 1) 0 :: ST s (FPlane s)
    let copyRow slot y = forLoop 0 (w - 1) $ \x -> rd p (y * stride + x) >>= wr rows (slot * w + x)
    copyRow 0 0
    forLoop 0 (h - 1) $ \y -> do
      let cu = y `mod` 3
          up = if y == 0 then cu else (y - 1) `mod` 3
          dn = if y + 1 >= h then cu else (y + 1) `mod` 3
      -- row y + 1 replaces row y - 2, which nothing needs any more
      when (y + 1 < h) $ copyRow ((y + 1) `mod` 3) (y + 1)
      forLoop 0 (w - 1) $ \x -> do
        let xl = if x == 0 then 0 else x - 1
            xr = if x + 1 >= w then w - 1 else x + 1
            at s xx = rd rows (s * w + xx)
        c0 <- at cu x
        l <- at cu xl; r <- at cu xr; u <- at up x; d <- at dn x
        ul <- at up xl; ur <- at up xr; dl <- at dn xl; dr <- at dn xr
        wr p (y * stride + x) ((c0 + (l + r + u + d) * w1 + (ul + ur + dl + dr) * w2) * g)

-- Non-overlapping contiguous Float ranges, indexed in samples. GHC's
-- byte-array primitive stays inside ST and preserves the exact Float bits.
{-# INLINE copyFloats #-}
copyFloats :: FPlane s -> Int -> FPlane s -> Int -> Int -> ST s ()
copyFloats (STUArray _ _ _ src#) so (STUArray _ _ _ dst#) dst count =
  case (so * 4, dst * 4, count * 4) of
    (I# so#, I# dstOff#, I# n#) -> ST $ \s# ->
      case copyMutableByteArray# src# so# dst# dstOff# n# s# of
        s1# -> (# s1#, () #)

-- | The edge-preserving filter over the w x h region (§J.3), in place.
-- @sigma@ gives each 8x8 block's sigma (stride @sw@); @iters@ selects the
-- steps: 3 runs step 0, 1 and 2; 2 runs 1 and 2; 1 runs step 1 only.
--
-- A kernel position's weight is a sum of absolute differences between the
-- pixels around it and around the centre, and neighbouring positions share
-- most of those differences: for an offset k, |I(q+k) - I(q)| (the D rows
-- below) is the same number as -k's difference at q+k. So each step works
-- in a pipeline of three row rings, indexed by *virtual* row (mirrored at
-- the frame edges): the unfiltered image (16 rows, padded with three
-- mirrored columns a side), D per channel for each offset pair, and S, D's
-- five-point cross sums, channel-weighted only after each channel's sum
-- is complete. The sums run in the order the direct formula takes, so the result
-- is bit-identical to evaluating every kernel in full (which cost five
-- times as much).
epf :: [FPlane s] -> Int -> Int -> Int -> FPlane s -> Int -> Int
    -> [Float] -> (Float, Float, Float) -> ST s ()
epf planes stride w h sigma sw iters chanScale (s0, s2, borderMul) = do
  let rw = w + 6
      npMax = if iters >= 3 then 6 else 2
  -- A D sample can influence an output at most three cells away: one
  -- cross-sum cell plus a pair offset of at most two. Conservatively mark
  -- each 8x8 block when any of its eight neighbours (or itself) filters.
  -- The mask is shared by all iterations and I/D/S; skipped ring slots
  -- cannot be read by a live output. Edge virtual cells use the edge block.
  let bw = (w + 7) `shiftR` 3
      bh = (h + 7) `shiftR` 3
  active <- newArray (0, bw * bh - 1) False :: ST s (STUArray s Int Bool)
  forLoop 0 (bh - 1) $ \by -> forLoop 0 (bw - 1) $ \bx -> do
    sg <- rd sigma (by * sw + bx)
    when (sg >= 0.3) $
      forLoop (max 0 (by - 1)) (min (bh - 1) (by + 1)) $ \ny ->
        forLoop (max 0 (bx - 1)) (min (bw - 1) (bx + 1)) $ \nx ->
          unsafeWrite active (ny * bw + nx) True
  rowSpans <- forM [0 .. bh - 1] $ \by -> do
    let gather !bx !start acc
          | bx >= bw = pure (reverse (if start < 0 then acc else (start, bw) : acc))
          | otherwise = do
              live <- unsafeRead active (by * bw + bx)
              if live then gather (bx + 1) (if start < 0 then bx else start) acc
                else gather (bx + 1) (-1) (if start < 0 then acc else (start, bx) : acc)
    gather 0 (-1) []
  unless (all null rowSpans) $ do
    ring <- newArray (0, 3 * 16 * rw - 1) 0 :: ST s (FPlane s)
    -- Steps 1 and 2 share the two offsets (1, 0) and (0, 1); their D and S
    -- rings interleave the pair and channel per sample ([row][x][pair][chan]
    -- and [row][x][pair]) so one output's six D or two S reads are adjacent.
    -- Step 0's six offsets keep the planar rings, which only it allocates.
    dring2 <- newArray (0, 8 * rw * 6 - 1) 0 :: ST s (FPlane s)
    sring2 <- newArray (0, 8 * rw * 2 - 1) 0 :: ST s (FPlane s)
    dring <- newArray (0, if iters >= 3 then npMax * 3 * 8 * rw - 1 else 0) 0 :: ST s (FPlane s)
    sring <- newArray (0, if iters >= 3 then npMax * 8 * rw - 1 else 0) 0 :: ST s (FPlane s)
    let spansByRow = A.listArray (0, bh - 1) rowSpans
        spanRuns r lo hi f =
          forM_ (spansByRow A.! max 0 (min (bh - 1) (r `shiftR` 3))) $ \(l, u) ->
            f (if l == 0 then lo else l * 8)
              (if u == bw then hi else u * 8 - 1)
        {-# INLINE spanRuns #-}
        spans r lo hi f = spanRuns r lo hi $ \l u -> forLoop l u f
        {-# INLINE spans #-}
    let [cs0, cs1, cs2] = chanScale
        [p0, p1, p2] = planes
        mirror n i | i < 0 = mirror n (negate i - 1)
                   | i >= n = mirror n (2 * n - 1 - i)
                   | otherwise = i
        wk :: Float -> Float -> Float -> Float -> Float
        wk negInv d0 d1 d2 = max 0 (1 + (cs0 * d0 + cs1 * d1 + cs2 * d2) * negInv)
        {-# INLINE wk #-}
        pairs = [(1, 0), (0, 1), (1, 1), (1, -1), (2, 0), (0, 2)] :: [(Int, Int)]
        -- virtual row r of the unfiltered image into the ring; rows past the
        -- bottom copy their mirror from the ring, since the planes' copy may
        -- already be filtered (rows above the top are read before anything is)
        loadI r = forM_ (zip planes [0 ..]) $ \(p, c) -> do
          let base = (c * 16 + (r .&. 15)) * rw + 3
          if r >= h
            then do
              let src = (c * 16 + (mirror h r .&. 15)) * rw
              spanRuns r (-3) (w + 2) $ \l u -> copyFloats ring (src + 3 + l) ring (base + l) (u - l + 1)
            else do
              let row = mirror h r * stride
              spanRuns r 0 (w - 1) $ \l u -> copyFloats p (row + l) ring (base + l) (u - l + 1)
              forM_ [-3, -2, -1] $ \x -> rd p (row + mirror w x) >>= wr ring (base + x)
              forM_ [w, w + 1, w + 2] $ \x -> rd p (row + mirror w x) >>= wr ring (base + x)
        computeD np r = forM_ (zip [0 .. np - 1] pairs) $ \(j, (kx, ky)) -> forM_ [0, 1, 2 :: Int] $ \c -> do
          let db = ((j * 3 + c) * 8 + (r .&. 7)) * rw + 3
              i0 = (c * 16 + (r .&. 15)) * rw + 3
              i1 = (c * 16 + ((r + ky) .&. 15)) * rw + 3 + kx
          spans r (-3) w $ \x -> do
            a <- rd ring (i1 + x)
            b <- rd ring (i0 + x)
            wr dring (db + x) (abs (a - b))
        computeS np r = forLoop 0 (np - 1) $ \j -> do
          let sb = (j * 8 + (r .&. 7)) * rw + 3
              cross c x = do
                let du = ((j * 3 + c) * 8 + ((r - 1) .&. 7)) * rw + 3
                    dc = ((j * 3 + c) * 8 + (r .&. 7)) * rw + 3
                    dd = ((j * 3 + c) * 8 + ((r + 1) .&. 7)) * rw + 3
                u <- rd dring (du + x)
                rr <- rd dring (dc + x + 1)
                ce <- rd dring (dc + x)
                l <- rd dring (dc + x - 1)
                d <- rd dring (dd + x)
                pure ((((u + rr) + ce) + l) + d)
              {-# INLINE cross #-}
          spans r (-2) (w - 1) $ \x -> do
            d0 <- cross 0 x
            d1 <- cross 1 x
            d2 <- cross 2 x
            wr sring (sb + x) (cs0 * d0 + cs1 * d1 + cs2 * d2)
        computeD1 r = do
          let !i0b0 = (0 * 16 + (r .&. 15)) * rw + 3
              !i0b1 = (1 * 16 + (r .&. 15)) * rw + 3
              !i0b2 = (2 * 16 + (r .&. 15)) * rw + 3
              !i1b0 = (0 * 16 + ((r + 1) .&. 15)) * rw + 3
              !i1b1 = (1 * 16 + ((r + 1) .&. 15)) * rw + 3
              !i1b2 = (2 * 16 + ((r + 1) .&. 15)) * rw + 3
              !db = ((r .&. 7) * rw + 3) * 6
          spans r (-3) w $ \x -> do
            let !o = db + x * 6
            b0 <- rd ring (i0b0 + x); a0 <- rd ring (i0b0 + x + 1); wr dring2 o (abs (a0 - b0))
            b1 <- rd ring (i0b1 + x); a1 <- rd ring (i0b1 + x + 1); wr dring2 (o + 1) (abs (a1 - b1))
            b2 <- rd ring (i0b2 + x); a2 <- rd ring (i0b2 + x + 1); wr dring2 (o + 2) (abs (a2 - b2))
            c0 <- rd ring (i1b0 + x); wr dring2 (o + 3) (abs (c0 - b0))
            c1 <- rd ring (i1b1 + x); wr dring2 (o + 4) (abs (c1 - b1))
            c2 <- rd ring (i1b2 + x); wr dring2 (o + 5) (abs (c2 - b2))
        computeS1 r = do
          let !du = (((r - 1) .&. 7) * rw + 3) * 6
              !dc = ((r .&. 7) * rw + 3) * 6
              !dd = (((r + 1) .&. 7) * rw + 3) * 6
              !sb = ((r .&. 7) * rw + 3) * 2
          spans r (-2) (w - 1) $ \x -> do
            let !ou = du + x * 6
                !oc = dc + x * 6
                !od = dd + x * 6
                cross k = do
                  u <- rd dring2 (ou + k)
                  rr <- rd dring2 (oc + 6 + k)
                  ce <- rd dring2 (oc + k)
                  l <- rd dring2 (oc - 6 + k)
                  d <- rd dring2 (od + k)
                  pure ((((u + rr) + ce) + l) + d)
                {-# INLINE cross #-}
            d0 <- cross 0; d1 <- cross 1; d2 <- cross 2
            wr sring2 (sb + x * 2) (cs0 * d0 + cs1 * d1 + cs2 * d2)
            e0 <- cross 3; e1 <- cross 4; e2 <- cross 5
            wr sring2 (sb + x * 2 + 1) (cs0 * e0 + cs1 * e1 + cs2 * e2)
        pixRow0 y mulStep = do
          let yBorder = ((y + 1) .&. 6) == 0
              !bI0_0 = (((0 * 16) + ((y + (0)) .&. 15)) * rw + 3)
              !bI1_0 = (((1 * 16) + ((y + (0)) .&. 15)) * rw + 3)
              !bI2_0 = (((2 * 16) + ((y + (0)) .&. 15)) * rw + 3)
              !bS50_m2 = ((5 * 8 + ((y + (-2)) .&. 7)) * rw + 3)
              !bI0_m2 = (((0 * 16) + ((y + (-2)) .&. 15)) * rw + 3)
              !bI1_m2 = (((1 * 16) + ((y + (-2)) .&. 15)) * rw + 3)
              !bI2_m2 = (((2 * 16) + ((y + (-2)) .&. 15)) * rw + 3)
              !bS20_m1 = ((2 * 8 + ((y + (-1)) .&. 7)) * rw + 3)
              !bI0_m1 = (((0 * 16) + ((y + (-1)) .&. 15)) * rw + 3)
              !bI1_m1 = (((1 * 16) + ((y + (-1)) .&. 15)) * rw + 3)
              !bI2_m1 = (((2 * 16) + ((y + (-1)) .&. 15)) * rw + 3)
              !bS10_m1 = ((1 * 8 + ((y + (-1)) .&. 7)) * rw + 3)
              !bS30_0 = ((3 * 8 + ((y + (0)) .&. 7)) * rw + 3)
              !bS40_0 = ((4 * 8 + ((y + (0)) .&. 7)) * rw + 3)
              !bS00_0 = ((0 * 8 + ((y + (0)) .&. 7)) * rw + 3)
              !bS30_1 = ((3 * 8 + ((y + (1)) .&. 7)) * rw + 3)
              !bI0_1 = (((0 * 16) + ((y + (1)) .&. 15)) * rw + 3)
              !bI1_1 = (((1 * 16) + ((y + (1)) .&. 15)) * rw + 3)
              !bI2_1 = (((2 * 16) + ((y + (1)) .&. 15)) * rw + 3)
              !bS10_0 = ((1 * 8 + ((y + (0)) .&. 7)) * rw + 3)
              !bS20_0 = ((2 * 8 + ((y + (0)) .&. 7)) * rw + 3)
              !bS50_0 = ((5 * 8 + ((y + (0)) .&. 7)) * rw + 3)
              !bI0_2 = (((0 * 16) + ((y + (2)) .&. 15)) * rw + 3)
              !bI1_2 = (((1 * 16) + ((y + (2)) .&. 15)) * rw + 3)
              !bI2_2 = (((2 * 16) + ((y + (2)) .&. 15)) * rw + 3)
          forLoop 0 (w - 1) $ \x -> do
            sg <- rd sigma ((y `shiftR` 3) * sw + (x `shiftR` 3))
            when (sg >= 0.3) $ do
                let xm = x .&. 7
                    sm = if yBorder || xm == 0 || xm == 7 then mulStep * borderMul else mulStep
                    !negInv = 6.6 * (0.7071067811865476 - 1) / sg * sm
                c0 <- rd ring (bI0_0 + x)
                c1 <- rd ring (bI1_0 + x)
                c2 <- rd ring (bI2_0 + x)
                d0_0 <- rd sring (bS50_m2 + x + (0))
                v0_0 <- rd ring (bI0_m2 + x + (0))
                v0_1 <- rd ring (bI1_m2 + x + (0))
                v0_2 <- rd ring (bI2_m2 + x + (0))
                let !w0 = max 0 (1 + d0_0 * negInv)
                d1_0 <- rd sring (bS20_m1 + x + (-1))
                v1_0 <- rd ring (bI0_m1 + x + (-1))
                v1_1 <- rd ring (bI1_m1 + x + (-1))
                v1_2 <- rd ring (bI2_m1 + x + (-1))
                let !w1 = max 0 (1 + d1_0 * negInv)
                d2_0 <- rd sring (bS10_m1 + x + (0))
                v2_0 <- rd ring (bI0_m1 + x + (0))
                v2_1 <- rd ring (bI1_m1 + x + (0))
                v2_2 <- rd ring (bI2_m1 + x + (0))
                let !w2 = max 0 (1 + d2_0 * negInv)
                d3_0 <- rd sring (bS30_0 + x + (0))
                v3_0 <- rd ring (bI0_m1 + x + (1))
                v3_1 <- rd ring (bI1_m1 + x + (1))
                v3_2 <- rd ring (bI2_m1 + x + (1))
                let !w3 = max 0 (1 + d3_0 * negInv)
                d4_0 <- rd sring (bS40_0 + x + (-2))
                v4_0 <- rd ring (bI0_0 + x + (-2))
                v4_1 <- rd ring (bI1_0 + x + (-2))
                v4_2 <- rd ring (bI2_0 + x + (-2))
                let !w4 = max 0 (1 + d4_0 * negInv)
                d5_0 <- rd sring (bS00_0 + x + (-1))
                v5_0 <- rd ring (bI0_0 + x + (-1))
                v5_1 <- rd ring (bI1_0 + x + (-1))
                v5_2 <- rd ring (bI2_0 + x + (-1))
                let !w5 = max 0 (1 + d5_0 * negInv)
                d6_0 <- rd sring (bS00_0 + x + (0))
                v6_0 <- rd ring (bI0_0 + x + (1))
                v6_1 <- rd ring (bI1_0 + x + (1))
                v6_2 <- rd ring (bI2_0 + x + (1))
                let !w6 = max 0 (1 + d6_0 * negInv)
                d7_0 <- rd sring (bS40_0 + x + (0))
                v7_0 <- rd ring (bI0_0 + x + (2))
                v7_1 <- rd ring (bI1_0 + x + (2))
                v7_2 <- rd ring (bI2_0 + x + (2))
                let !w7 = max 0 (1 + d7_0 * negInv)
                d8_0 <- rd sring (bS30_1 + x + (-1))
                v8_0 <- rd ring (bI0_1 + x + (-1))
                v8_1 <- rd ring (bI1_1 + x + (-1))
                v8_2 <- rd ring (bI2_1 + x + (-1))
                let !w8 = max 0 (1 + d8_0 * negInv)
                d9_0 <- rd sring (bS10_0 + x + (0))
                v9_0 <- rd ring (bI0_1 + x + (0))
                v9_1 <- rd ring (bI1_1 + x + (0))
                v9_2 <- rd ring (bI2_1 + x + (0))
                let !w9 = max 0 (1 + d9_0 * negInv)
                d10_0 <- rd sring (bS20_0 + x + (0))
                v10_0 <- rd ring (bI0_1 + x + (1))
                v10_1 <- rd ring (bI1_1 + x + (1))
                v10_2 <- rd ring (bI2_1 + x + (1))
                let !w10 = max 0 (1 + d10_0 * negInv)
                d11_0 <- rd sring (bS50_0 + x + (0))
                v11_0 <- rd ring (bI0_2 + x + (0))
                v11_1 <- rd ring (bI1_2 + x + (0))
                v11_2 <- rd ring (bI2_2 + x + (0))
                let !w11 = max 0 (1 + d11_0 * negInv)
                let !sw = 1 + w0 + w1 + w2 + w3 + w4 + w5 + w6 + w7 + w8 + w9 + w10 + w11
                    !t0 = c0 + w0 * v0_0 + w1 * v1_0 + w2 * v2_0 + w3 * v3_0 + w4 * v4_0 + w5 * v5_0 + w6 * v6_0 + w7 * v7_0 + w8 * v8_0 + w9 * v9_0 + w10 * v10_0 + w11 * v11_0
                    !t1 = c1 + w0 * v0_1 + w1 * v1_1 + w2 * v2_1 + w3 * v3_1 + w4 * v4_1 + w5 * v5_1 + w6 * v6_1 + w7 * v7_1 + w8 * v8_1 + w9 * v9_1 + w10 * v10_1 + w11 * v11_1
                    !t2 = c2 + w0 * v0_2 + w1 * v1_2 + w2 * v2_2 + w3 * v3_2 + w4 * v4_2 + w5 * v5_2 + w6 * v6_2 + w7 * v7_2 + w8 * v8_2 + w9 * v9_2 + w10 * v10_2 + w11 * v11_2
                    o = y * stride + x
                wr p0 o (t0 / sw)
                wr p1 o (t1 / sw)
                wr p2 o (t2 / sw)
        pixRow1 y mulStep = do
          let yBorder = ((y + 1) .&. 6) == 0
              !bI0_0 = (((0 * 16) + ((y + (0)) .&. 15)) * rw + 3)
              !bI1_0 = (((1 * 16) + ((y + (0)) .&. 15)) * rw + 3)
              !bI2_0 = (((2 * 16) + ((y + (0)) .&. 15)) * rw + 3)
              !bSm1 = (((y - 1) .&. 7) * rw + 3) * 2
              !bI0_m1 = (((0 * 16) + ((y + (-1)) .&. 15)) * rw + 3)
              !bI1_m1 = (((1 * 16) + ((y + (-1)) .&. 15)) * rw + 3)
              !bI2_m1 = (((2 * 16) + ((y + (-1)) .&. 15)) * rw + 3)
              !bS0 = ((y .&. 7) * rw + 3) * 2
              !bI0_1 = (((0 * 16) + ((y + (1)) .&. 15)) * rw + 3)
              !bI1_1 = (((1 * 16) + ((y + (1)) .&. 15)) * rw + 3)
              !bI2_1 = (((2 * 16) + ((y + (1)) .&. 15)) * rw + 3)
          forLoop 0 (w - 1) $ \x -> do
            sg <- rd sigma ((y `shiftR` 3) * sw + (x `shiftR` 3))
            when (sg >= 0.3) $ do
                let xm = x .&. 7
                    sm = if yBorder || xm == 0 || xm == 7 then mulStep * borderMul else mulStep
                    !negInv = 6.6 * (0.7071067811865476 - 1) / sg * sm
                c0 <- rd ring (bI0_0 + x)
                c1 <- rd ring (bI1_0 + x)
                c2 <- rd ring (bI2_0 + x)
                d0_0 <- rd sring2 (bSm1 + x * 2 + 1)
                v0_0 <- rd ring (bI0_m1 + x + (0))
                v0_1 <- rd ring (bI1_m1 + x + (0))
                v0_2 <- rd ring (bI2_m1 + x + (0))
                let !w0 = max 0 (1 + d0_0 * negInv)
                d1_0 <- rd sring2 (bS0 + x * 2 + 1)
                v1_0 <- rd ring (bI0_1 + x + (0))
                v1_1 <- rd ring (bI1_1 + x + (0))
                v1_2 <- rd ring (bI2_1 + x + (0))
                let !w1 = max 0 (1 + d1_0 * negInv)
                d2_0 <- rd sring2 (bS0 + x * 2 - 2)
                v2_0 <- rd ring (bI0_0 + x + (-1))
                v2_1 <- rd ring (bI1_0 + x + (-1))
                v2_2 <- rd ring (bI2_0 + x + (-1))
                let !w2 = max 0 (1 + d2_0 * negInv)
                d3_0 <- rd sring2 (bS0 + x * 2)
                v3_0 <- rd ring (bI0_0 + x + (1))
                v3_1 <- rd ring (bI1_0 + x + (1))
                v3_2 <- rd ring (bI2_0 + x + (1))
                let !w3 = max 0 (1 + d3_0 * negInv)
                let !sw = 1 + w0 + w1 + w2 + w3
                    !t0 = c0 + w0 * v0_0 + w1 * v1_0 + w2 * v2_0 + w3 * v3_0
                    !t1 = c1 + w0 * v0_1 + w1 * v1_1 + w2 * v2_1 + w3 * v3_1
                    !t2 = c2 + w0 * v0_2 + w1 * v1_2 + w2 * v2_2 + w3 * v3_2
                    o = y * stride + x
                wr p0 o (t0 / sw)
                wr p1 o (t1 / sw)
                wr p2 o (t2 / sw)
        pixRow2 y mulStep = do
          let yBorder = ((y + 1) .&. 6) == 0
              !bI0_0 = (((0 * 16) + ((y + (0)) .&. 15)) * rw + 3)
              !bI1_0 = (((1 * 16) + ((y + (0)) .&. 15)) * rw + 3)
              !bI2_0 = (((2 * 16) + ((y + (0)) .&. 15)) * rw + 3)
              !bDm1 = (((y - 1) .&. 7) * rw + 3) * 6
              !bI0_m1 = (((0 * 16) + ((y + (-1)) .&. 15)) * rw + 3)
              !bI1_m1 = (((1 * 16) + ((y + (-1)) .&. 15)) * rw + 3)
              !bI2_m1 = (((2 * 16) + ((y + (-1)) .&. 15)) * rw + 3)
              !bD0 = ((y .&. 7) * rw + 3) * 6
              !bI0_1 = (((0 * 16) + ((y + (1)) .&. 15)) * rw + 3)
              !bI1_1 = (((1 * 16) + ((y + (1)) .&. 15)) * rw + 3)
              !bI2_1 = (((2 * 16) + ((y + (1)) .&. 15)) * rw + 3)
          forLoop 0 (w - 1) $ \x -> do
            sg <- rd sigma ((y `shiftR` 3) * sw + (x `shiftR` 3))
            when (sg >= 0.3) $ do
                let xm = x .&. 7
                    sm = if yBorder || xm == 0 || xm == 7 then mulStep * borderMul else mulStep
                    !negInv = 6.6 * (0.7071067811865476 - 1) / sg * sm
                c0 <- rd ring (bI0_0 + x)
                c1 <- rd ring (bI1_0 + x)
                c2 <- rd ring (bI2_0 + x)
                d0_0 <- rd dring2 (bDm1 + x * 6 + 3)
                d0_1 <- rd dring2 (bDm1 + x * 6 + 4)
                d0_2 <- rd dring2 (bDm1 + x * 6 + 5)
                v0_0 <- rd ring (bI0_m1 + x + (0))
                v0_1 <- rd ring (bI1_m1 + x + (0))
                v0_2 <- rd ring (bI2_m1 + x + (0))
                let !w0 = wk negInv d0_0 d0_1 d0_2
                d1_0 <- rd dring2 (bD0 + x * 6 + 3)
                d1_1 <- rd dring2 (bD0 + x * 6 + 4)
                d1_2 <- rd dring2 (bD0 + x * 6 + 5)
                v1_0 <- rd ring (bI0_1 + x + (0))
                v1_1 <- rd ring (bI1_1 + x + (0))
                v1_2 <- rd ring (bI2_1 + x + (0))
                let !w1 = wk negInv d1_0 d1_1 d1_2
                d2_0 <- rd dring2 (bD0 + x * 6 - 6)
                d2_1 <- rd dring2 (bD0 + x * 6 - 5)
                d2_2 <- rd dring2 (bD0 + x * 6 - 4)
                v2_0 <- rd ring (bI0_0 + x + (-1))
                v2_1 <- rd ring (bI1_0 + x + (-1))
                v2_2 <- rd ring (bI2_0 + x + (-1))
                let !w2 = wk negInv d2_0 d2_1 d2_2
                d3_0 <- rd dring2 (bD0 + x * 6)
                d3_1 <- rd dring2 (bD0 + x * 6 + 1)
                d3_2 <- rd dring2 (bD0 + x * 6 + 2)
                v3_0 <- rd ring (bI0_0 + x + (1))
                v3_1 <- rd ring (bI1_0 + x + (1))
                v3_2 <- rd ring (bI2_0 + x + (1))
                let !w3 = wk negInv d3_0 d3_1 d3_2
                let !sw = 1 + w0 + w1 + w2 + w3
                    !t0 = c0 + w0 * v0_0 + w1 * v1_0 + w2 * v2_0 + w3 * v3_0
                    !t1 = c1 + w0 * v0_1 + w1 * v1_1 + w2 * v2_1 + w3 * v3_1
                    !t2 = c2 + w0 * v0_2 + w1 * v1_2 + w2 * v2_2 + w3 * v3_2
                    o = y * stride + x
                wr p0 o (t0 / sw)
                wr p1 o (t1 / sw)
                wr p2 o (t2 / sw)
        step kind mulStep = do
          let np = if kind == 0 then 6 else 2
              cross = kind /= 2
          forLoop (-4) 4 loadI
          if kind /= 0 then forLoop (-3) 2 computeD1 else forLoop (-3) 2 (computeD np)
          when cross $ if kind == 1 then forLoop (-2) 1 computeS1 else forLoop (-2) 1 (computeS np)
          forLoop 0 (h - 1) $ \y -> do
            when (y > 0) $ do
              loadI (y + 4)
              if kind /= 0 then computeD1 (y + 2) else computeD np (y + 2)
              when cross $ if kind == 1 then computeS1 (y + 1) else computeS np (y + 1)
            case kind of
              0 -> pixRow0 y mulStep
              1 -> pixRow1 y mulStep
              _ -> pixRow2 y mulStep
    when (iters >= 3) $ step (0 :: Int) s0
    when (iters >= 1) $ step 1 1
    when (iters >= 2) $ step 2 s2

{-# INLINE forLoopStep #-}
forLoopStep :: Int -> Int -> Int -> (Int -> ST s ()) -> ST s ()
forLoopStep lo hi st f = go lo
  where go !i | i > hi = pure ()
              | otherwise = f i >> go (i + st)

{-# INLINE forLoop #-}
forLoop :: Int -> Int -> (Int -> ST s ()) -> ST s ()
forLoop lo hi f = go lo
  where go !i | i > hi = pure ()
              | otherwise = f i >> go (i + 1)

------------------------------------------------------------------------------
-- HF coefficients (§I.3.3)

-- | Block-context configuration for the HF coefficients of a frame.
data HfCtx = HfCtx
  { hcMap :: !(UArray Int Int)     -- ^ block context map
  , hcNbc :: !Int                  -- ^ number of block contexts
  , hcQfThr :: !(UArray Int Int)   -- ^ HfMul thresholds
  , hcLfMul :: !Int                -- ^ product of the LF threshold counts + 1
  }

coeffFreqCtx, coeffNnzCtx :: UArray Int Int
coeffFreqCtx = listArray (0, 62)
  [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 15, 16, 16, 17, 17, 18, 18, 19, 19
  , 20, 20, 21, 21, 22, 22, 23, 23, 23, 23, 24, 24, 24, 24, 25, 25, 25, 25, 26, 26, 26, 26, 27
  , 27, 27, 27, 28, 28, 28, 28, 29, 29, 29, 29, 30, 30, 30, 30 ]
coeffNnzCtx = listArray (0, 62)
  [ 0, 31, 62, 62, 93, 93, 93, 93, 123, 123, 123, 123, 152, 152, 152, 152, 152, 152, 152, 152
  , 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 206, 206, 206, 206, 206, 206
  , 206, 206, 206, 206, 206, 206, 206, 206, 206, 206, 206, 206, 206, 206, 206, 206, 206, 206
  , 206, 206, 206, 206, 206, 206, 206 ]

-- | Decode one pass group's HF coefficients, adding them (shifted) into the
-- integer views of the three coefficient planes. Blocks are addressed in
-- frame block coordinates; @blk@ holds, per 8x8 block, @t | hfMul << 8@ for
-- a varblock's top-left block and a negative value otherwise.
decodeHfGroup :: BR s -> Code -> CodeSt s -> Int -> HfCtx -> Array Int (UArray Int Int)
              -> STUArray s Int Int32 -> STUArray s Int Int32 -> Int
              -> [STUArray s Int Int32] -> [Int] -> [(Int, Int)] -> Int -> Int -> Int -> Int -> Int -> J s ()
decodeHfGroup br code cs ctxOff hc orders blk lfIdx bw planes pws shifts bx0 by0 nbx nby shift = do
  nzRow <- liftJ (newArray (0, 3 * nbx) 0) :: J s (STUArray s Int Int)
  err <- liftJ (newArray (0, 0) 0) :: J s (STUArray s Int Int)
  let nbc = hcNbc hc
      qfThr = hcQfThr hc
      nqf = snd (U.bounds qfThr) + 1
      hfMul = nqf + 1
      lfMul = hcLfMul hc
      bmap = hcMap hc
  let {-# INLINE run #-}
      run sr = do
        forLoop 0 (nby - 1) $ \ly -> forLoop 0 (nbx - 1) $ \lx -> do
          let by = by0 + ly
              bx = bx0 + lx
              bi = by * bw + bx
          e <- fromIntegral <$> unsafeRead blk bi
          when (e >= 0) $ do
            lfi <- fromIntegral <$> unsafeRead lfIdx bi
            let t = e .&. 0xff
                qf = e `shiftR` 8
                w8 = dctW8 `unsafeAt` t
                h8 = dctH8 `unsafeAt` t
                nb = w8 * h8
                lognb = countTrailingZeros nb
                oid = dctOrder `unsafeAt` t
                tr = dctTranspose t
                size = (orderBW `unsafeAt` oid) * (orderBH `unsafeAt` oid)
                hfi = length [ () | i <- [0 .. nqf - 1], qf > qfThr `unsafeAt` i ]
            forLoop 0 2 $ \cy -> do
              let c = if cy == 0 then 1 else if cy == 1 then 0 else 2
                  (hs, vs) = shifts !! c
                  -- a subsampled channel takes the blocks at multiples of its
                  -- shift, and only where a varblock starts there too
                  slx = lx `shiftR` hs
                  sly = ly `shiftR` vs
                  sbx = bx `shiftR` hs
                  sby = by `shiftR` vs
              okPos <- if hs == 0 && vs == 0 then pure True
                       else if (slx `shiftL` hs) /= lx || (sly `shiftL` vs) /= ly then pure False
                       else (>= 0) <$> unsafeRead blk ((by0 + sly) * bw + bx0 + slx)
              when okPos $ do
               let bctx = bmap `unsafeAt` ((((cy * 13 + oid) * hfMul) + hfi) * lfMul + lfi)
                   rowBase = c * nbx
               left <- if slx > 0 then unsafeRead nzRow (rowBase + slx - 1) else pure 0
               above <- unsafeRead nzRow (rowBase + slx)
               let pred | sly == 0 = if slx == 0 then 32 else left
                        | slx == 0 = above
                        | otherwise = (above + left + 1) `shiftR` 1
                   nzctx = bctx + (if pred < 8 then pred else 4 + pred `shiftR` 1) * nbc
               !nz0 <- readWith sr (clusterOf code (ctxOff + nzctx))
               if nz0 > (63 `shiftL` lognb) then unsafeWrite err 0 1 else do
                let nzv = (nz0 + nb - 1) `shiftR` lognb
                forLoop 0 (w8 - 1) $ \dx -> when (slx + dx < nbx) $ unsafeWrite nzRow (rowBase + slx + dx) nzv
                when (nz0 > 0) $ do
                  let order = orders A.! (oid * 3 + c)
                      base = bctx * 458 + 37 * nbc
                      plane = planes !! c
                      pw = pws !! c
                      origin = sby * 8 * pw + sbx * 8
                      go !k !nz !prev
                        | nz <= 0 = pure ()
                        | k >= size = unsafeWrite err 0 1
                        | otherwise = do
                            let cctx = (coeffNnzCtx `unsafeAt` ((nz - 1) `shiftR` lognb)
                                        + coeffFreqCtx `unsafeAt` ((k - nb) `shiftR` lognb)) * 2 + prev
                            !u <- readWith sr (clusterOf code (ctxOff + base + min 457 cctx))
                            if cctx >= 458 then unsafeWrite err 0 1
                            else if u == 0 then go (k + 1) nz 0 else do
                              let co = order `unsafeAt` k
                                  ox = co .&. 0xffff
                                  oy = co `shiftR` 16
                                  (dx, dy) = if tr then (oy, ox) else (ox, oy)
                                  pos = origin + dy * pw + dx
                              old <- unsafeRead plane pos
                              unsafeWrite plane pos (old + fromIntegral (unpackSigned u `shiftL` shift))
                              go (k + 1) (nz - 1) 1
                  go nb nz0 (if nz0 <= nb * 4 then 1 else 0)
  liftJ $ withSymReader code cs br 0 run
  e <- liftJ (unsafeRead err 0)
  jguard (e == 0) "JPEG XL: corrupt HF coefficients"

-- | Filter one 128-row band at a time, with private 512-pixel tiles.
-- The three input bands are immutable while workers run. Before refilling
-- them, retain the previous core's last eight UNFILTERED rows; the main
-- planes now contain filtered pixels there. Eight aligned halo pixels cover
-- gaborish (1) plus EPF steps 0/1/2 (3+2+1), without changing sigma borders.
-- At most four tile jobs are live. Each finishes before its output is copied
-- on the caller thread, and every worker is joined before reusing the band.
-- This bounded consumer still needs the original full reconstructed planes.
filterBands :: Int -> [FPlane s] -> Int -> Int -> Int -> FPlane s -> Int
            -> Maybe [Double] -> Int -> [Float] -> (Float, Float, Float) -> ST s ()
filterBands workers planes stride w h sigma sw gab iters scale params = do
  let bandH = 128
      tileW = 512
      count = (w + tileW - 1) `div` tileW
      slots = max 1 (min workers count)
      maxW = min w (tileW + 16)
      maxH = min h (bandH + 16)
  -- A job k leases slot k mod slots. The scheduler returns a credit only
  -- AFTER consuming k, so k+slots cannot reuse its arrays during the copy.
  -- Reuse these buffers across bands too; avoid accumulating tile garbage.
  pool <- replicateM slots $ do
    ps <- replicateM 3 (newArray (0, maxW * maxH - 1) 0)
    sig <- newArray (0, ((maxW + 7) `div` 8) * ((maxH + 7) `div` 8) - 1) 0
    pure (ps, sig)
  source <- replicateM 3 (newArray (0, (bandH + 16) * w - 1) 0)
  let bands !y0
        | y0 >= h = pure ()
        | otherwise = do
            let top = max 0 (y0 - 8)
                end = min h (y0 + bandH)
                bottom = min h (end + 8)
                ch = bottom - top
                sh = (ch + 7) `div` 8
            -- The previous band's local origin is max 0 (y0-bandH-8).
            -- Its last eight core rows become this band's first eight rows.
            forM_ (zip source planes) $ \(src, dst) -> do
              when (y0 > 0) $
                copyFloats src ((y0 - 8 - max 0 (y0 - bandH - 8)) * w) src 0 (8 * w)
              forLoop y0 (bottom - 1) $ \y ->
                copyFloats dst (y * stride) src ((y - top) * w) w
            let job k = do
                  let x0 = k * tileW
                      x1 = min w (x0 + tileW)
                      left = max 0 (x0 - 8)
                      right = min w (x1 + 8)
                      cw = right - left
                      sx = (cw + 7) `div` 8
                      (ps, sig) = pool !! (k `mod` slots)
                  forM_ (zip source ps) $ \(src, dst) ->
                    forLoop 0 (ch - 1) $ \y -> copyFloats src (y * w + left) dst (y * cw) cw
                  forLoop 0 (sh - 1) $ \y ->
                    copyFloats sigma ((top `div` 8 + y) * sw + left `div` 8) sig (y * sx) sx
                  maybe (pure ()) (gaborish ps cw cw ch) gab
                  when (iters > 0) (epf ps cw cw ch sig sx iters scale params)
                  pure ps
                consume k ps = do
                  let x0 = k * tileW
                      x1 = min w (x0 + tileW)
                      left = max 0 (x0 - 8)
                      cw = min w (x1 + 8) - left
                  forM_ (zip ps planes) $ \(src, dst) ->
                    forLoop y0 (end - 1) $ \y ->
                      copyFloats src ((y - top) * cw + x0 - left) dst (y * stride + x0) (x1 - x0)
                  pure True
            unsafeIOToST $ parOrdered slots slots count
              (unsafeSTToIO . job) (\k -> unsafeSTToIO . consume k)
            bands end
  bands 0
