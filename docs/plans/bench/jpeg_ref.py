"""Executable spec: libjpeg-turbo default decode (ISLOW, fancy upsampling, jdcolor
tables, scale 1/1 1/2 1/4 1/8) for baseline Huffman JPEGs, in plain Python.
Usage: ref.py file.jpg [scale]  -> compares against Pillow (draft for scale>1)."""
import sys, struct
import numpy as np
from PIL import Image
ZZ=[0,1,8,16,9,2,3,10,17,24,32,25,18,11,4,5,12,19,26,33,40,48,41,34,27,20,13,6,7,14,21,28,
    35,42,49,56,57,50,43,36,29,22,15,23,30,37,44,51,58,59,52,45,38,31,39,46,53,60,61,54,47,55,62,63]
def parse(b):
    p=2; q={}; hd={}; ha={}; fr=None; dri=0; adobe=None; jfif=False
    while True:
        assert b[p]==0xFF; m=b[p+1]; L=struct.unpack('>H',b[p+2:p+4])[0]; s=b[p+4:p+2+L]
        if m==0xDB:
            i=0
            while i<len(s):
                pq,tq=s[i]>>4,s[i]&15; t=[0]*64
                for k in range(64):
                    t[ZZ[k]]=s[i+1+k] if pq==0 else struct.unpack('>H',s[i+1+2*k:i+3+2*k])[0]
                q[tq]=t; i+=65 if pq==0 else 129
        elif m==0xC4:
            i=0
            while i<len(s):
                tc,th=s[i]>>4,s[i]&15; cnt=list(s[i+1:i+17]); n=sum(cnt); syms=list(s[i+17:i+17+n])
                tab={}; code=0; k=0
                for ln in range(1,17):
                    for _ in range(cnt[ln-1]): tab[(ln,code)]=syms[k]; k+=1; code+=1
                    code<<=1
                (hd if tc==0 else ha)[th]=tab; i+=17+n
        elif m in (0xC0,0xC1):
            h,w,nf=struct.unpack('>xHHB',s[:6]); fr=(w,h,[(s[6+3*i],s[7+3*i]>>4,s[7+3*i]&15,s[8+3*i]) for i in range(nf)])
        elif m==0xC2: raise SystemExit('progressive: not in this reference')
        elif m==0xDD: dri=struct.unpack('>H',s[:2])[0]
        elif m==0xEE and s[:5]==b'Adobe': adobe=s[11]
        elif m==0xE0 and s[:4]==b'JFIF': jfif=True
        elif m==0xDA:
            ns=s[0]; sc=[(s[1+2*i],s[2+2*i]>>4,s[2+2*i]&15) for i in range(ns)]
            return q,hd,ha,fr,dri,adobe,jfif,sc,p+2+L
        p+=2+L
class BR:
    def __init__(s,b,p): s.b=b; s.p=p; s.acc=0; s.n=0
    def bit(s):
        if s.n==0:
            c=s.b[s.p]
            if c==0xFF:
                if s.b[s.p+1]==0: s.p+=2
                else: c=0          # marker: feed zeros
            else: s.p+=1
            s.acc=c; s.n=8
        s.n-=1; return (s.acc>>s.n)&1
    def bits(s,k):
        v=0
        for _ in range(k): v=(v<<1)|s.bit()
        return v
    def huff(s,t):
        code=0
        for ln in range(1,17):
            code=(code<<1)|s.bit()
            if (ln,code) in t: return t[(ln,code)]
        raise ValueError
    def recv(s,k):
        if k==0: return 0
        v=s.bits(k); return v if v>=(1<<(k-1)) else v-(1<<k)+1
    def restart(s):
        s.n=0
        while not (s.b[s.p]==0xFF and 0xD0<=s.b[s.p+1]<=0xD7): s.p+=1
        s.p+=2
D=lambda x,n:(x+(1<<(n-1)))>>n
clampv=lambda x:0 if x<-128 else 255 if x>127 else x+128     # SIMD saturates; C wraps via RANGE_MASK
def islow(c):          # c: 64 dequantised ints, natural order -> 8x8 samples
    ws=[0]*64
    def k1d(i0,i1,i2,i3,i4,i5,i6,i7):
        z1=(i2+i6)*4433; t2=z1-i6*15137; t3=z1+i2*6270
        t0=(i0+i4)<<13; t1=(i0-i4)<<13
        t10=t0+t3; t13=t0-t3; t11=t1+t2; t12=t1-t2
        a0,a1,a2,a3=i7,i5,i3,i1
        z1=a0+a3; z2=a1+a2; z3=a0+a2; z4=a1+a3; z5=(z3+z4)*9633
        a0*=2446; a1*=16819; a2*=25172; a3*=12299
        z1*=-7373; z2*=-20995; z3=z3*-16069+z5; z4=z4*-3196+z5
        a0+=z1+z3; a1+=z2+z4; a2+=z2+z3; a3+=z1+z4
        return (t10+a3,t11+a2,t12+a1,t13+a0,t13-a0,t12-a1,t11-a2,t10-a3)
    for x in range(8):
        o=k1d(*[c[r*8+x] for r in range(8)])
        for r in range(8): ws[r*8+x]=D(o[r],11)
    out=[]
    for r in range(8):
        o=k1d(*ws[r*8:r*8+8]); out.append([clampv(D(v,18)) for v in o])
    return out
def idct4(c):
    ws=[0]*32
    def k(i0,i1,i2,i3,i5,i6,i7):
        t0=i0<<14; t2=i2*15137-i6*6270; t10=t0+t2; t12=t0-t2
        a0=-i7*1730+i5*11893-i3*17799+i1*8697
        a2=-i7*4176-i5*4926+i3*7373+i1*20995
        return (t10+a2,t12+a0,t12-a0,t10-a2)
    for x in (0,1,2,3,5,6,7):
        o=k(*[c[r*8+x] for r in (0,1,2,3,5,6,7)])
        for r in range(4): ws[r*8+x]=D(o[r],12)
    return [[clampv(D(v,19)) for v in k(*[ws[r*8+x] for x in (0,1,2,3,5,6,7)])] for r in range(4)]
def idct2(c):
    ws=[0]*16
    def k(i0,i1,i3,i5,i7):
        t10=i0<<15; t0=-i7*5906+i5*6967-i3*10426+i1*29692
        return (t10+t0,t10-t0)
    for x in (0,1,3,5,7):
        o=k(*[c[r*8+x] for r in (0,1,3,5,7)])
        for r in range(2): ws[r*8+x]=D(o[r],13)
    return [[clampv(D(v,20)) for v in k(*[ws[r*8+x] for x in (0,1,3,5,7)])] for r in range(2)]
def idct1(c): return [[clampv(D(c[0],3))]]
IDCT={8:islow,4:idct4,2:idct2,1:idct1}
def decode(path,scale=1):
    b=open(path,'rb').read(); q,hd,ha,(w,h,comps),dri,adobe,jfif,sc,p=parse(b)
    hmax=max(c[1] for c in comps); vmax=max(c[2] for c in comps)
    mpr=-(-w//(8*hmax)); mpc=-(-h//(8*vmax)); mind=8//scale
    br=BR(b,p); pred=[0]*len(comps); planes=[]; dsz=[]
    for (cid,ch,cv,tq) in comps:          # jdmaster: scale chroma up in the IDCT when possible
        s=mind
        while s<8 and (hmax*mind)%(ch*s*2)==0 and (vmax*mind)%(cv*s*2)==0: s*=2
        dsz.append(s); planes.append(np.zeros((mpc*cv*s,mpr*ch*s),dtype=np.int32))
    order={cid:i for i,(cid,_,_,_) in enumerate(comps)}
    assert len(sc)==len(comps), 'non-interleaved scans not in this reference'
    n=0
    for my in range(mpc):
        for mx in range(mpr):
            if dri and n and n%dri==0: br.restart(); pred=[0]*len(comps)
            n+=1
            for (cid,td,ta) in sc:
                ci=order[cid]; _,ch,cv,tq=comps[ci]; s=dsz[ci]
                for by in range(cv):
                    for bx in range(ch):
                        c=[0]*64; t=br.huff(hd[td]); pred[ci]+=br.recv(t); c[0]=pred[ci]*q[tq][0]; k=1
                        while k<64:
                            rs=br.huff(ha[ta]); r,sz=rs>>4,rs&15
                            if sz==0:
                                if r<15: break
                                k+=16; continue
                            k+=r; c[ZZ[k]]=br.recv(sz)*q[tq][ZZ[k]]; k+=1
                        y0=(my*cv+by)*s; x0=(mx*ch+bx)*s
                        planes[ci][y0:y0+s,x0:x0+s]=IDCT[s](c)
    ow=-(-w//scale); oh=-(-h//scale); up=[]
    for ci,(cid,ch,cv,tq) in enumerate(comps):
        s=dsz[ci]; dw=-(-(w*ch*s)//(hmax*8)); dh=-(-(h*cv*s)//(vmax*8))
        P=planes[ci][:dh,:dw]; he=(hmax*mind)//(ch*s); ve=(vmax*mind)//(cv*s)
        assert (hmax*mind)%(ch*s)==0 and (vmax*mind)%(cv*s)==0, 'fractional: libjpeg refuses'
        fancy=mind>1
        if he==1 and ve==1: U=P
        elif he==2 and ve==1 and fancy and dw>2: U=h2v1(P)
        elif he==2 and ve==2 and fancy and dw>2: U=h2v2(P)
        elif he==1 and ve==2 and fancy: U=h1v2(P)
        else: U=np.repeat(np.repeat(P,he,axis=1),ve,axis=0)
        up.append(U[:oh,:ow])
    if len(comps)==1: return np.stack([up[0]]*3,axis=2).astype(np.uint8)
    ids=[c[0] for c in comps]
    rgb = (adobe==0) if (adobe is not None and not jfif) else (not jfif and ids==[82,71,66])
    if rgb: return np.stack(up,axis=2).astype(np.uint8)
    Y,Cb,Cr=[u.astype(np.int64) for u in up]; x=np.arange(256)-128
    crr=(91881*x+32768)>>16; cbb=(116130*x+32768)>>16; crg=-46802*x; cbg=-22554*x+32768
    R=np.clip(Y+crr[Cr],0,255); G=np.clip(Y+((cbg[Cb]+crg[Cr])>>16),0,255); B=np.clip(Y+cbb[Cb],0,255)
    return np.stack([R,G,B],axis=2).astype(np.uint8)
def h2v1(P):
    P=P.astype(np.int64); l=np.concatenate([P[:,:1],P[:,:-1]],1); r=np.concatenate([P[:,1:],P[:,-1:]],1)
    O=np.empty((P.shape[0],P.shape[1]*2),np.int64); O[:,0::2]=(3*P+l+1)>>2; O[:,1::2]=(3*P+r+2)>>2
    O[:,0]=P[:,0]; O[:,-1]=P[:,-1]; return O
def h1v2(P):
    P=P.astype(np.int64); a=np.concatenate([P[:1],P[:-1]],0); b=np.concatenate([P[1:],P[-1:]],0)
    O=np.empty((P.shape[0]*2,P.shape[1]),np.int64); O[0::2]=(3*P+a+1)>>2; O[1::2]=(3*P+b+2)>>2; return O
def h2v2(P):
    P=P.astype(np.int64); a=np.concatenate([P[:1],P[:-1]],0); b=np.concatenate([P[1:],P[-1:]],0)
    O=np.empty((P.shape[0]*2,P.shape[1]*2),np.int64)
    for v,nb in ((0,a),(1,b)):
        S=3*P+nb; l=np.concatenate([S[:,:1],S[:,:-1]],1); r=np.concatenate([S[:,1:],S[:,-1:]],1)
        O[v::2,0::2]=(3*S+l+8)>>4; O[v::2,1::2]=(3*S+r+7)>>4
    return O
if __name__=='__main__':
    f=sys.argv[1]; s=int(sys.argv[2]) if len(sys.argv)>2 else 1
    im=Image.open(f)
    if s>1: im.draft(im.mode,(im.size[0]//s,im.size[1]//s))
    ref=np.asarray(im.convert('RGB')).astype(int); mine=decode(f,s).astype(int)
    if ref.shape!=mine.shape: print(f,s,'SHAPE',ref.shape,mine.shape)
    else:
        d=np.abs(ref-mine); print('%-32s 1/%d %s max %d differing %d'%(f.split('/')[-1],s,ref.shape[:2],d.max(),(d>0).sum()))
