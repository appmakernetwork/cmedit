#!/usr/bin/env python3
"""Second half of pdfrendercheck.sh: compare our pages with pdftoppm's.

    pdfrendercompare.py OUTDIR

Reads OUTDIR/keys.txt (key, path, page, ref status) and OUTDIR/ours.txt (the
bench's result lines), converts each of our PAMs to a PNG flattened on white,
and runs ImageMagick's compare twice: RMSE (normalised) and AE with a 15%
fuzz, as a percentage of the pixels. Sizes that differ by rounding are
compared over their common area. Writes OUTDIR/results.tsv and prints the
distribution plus the 20 worst pages.
"""
import os
import subprocess
import sys


def magick(*args):
    r = subprocess.run(['magick'] + list(args), capture_output=True, text=True, timeout=300)
    return r.returncode, r.stdout + r.stderr


def size_of(path):
    _, out = magick('identify', '-format', '%w %h', path)
    try:
        w, h = out.split()[:2]
        return int(w), int(h)
    except ValueError:
        return None


def main(out):
    ours = {}
    with open(os.path.join(out, 'ours.txt'), errors='replace') as fh:
        for line in fh:
            parts = line.rstrip('\n').split('\t')
            if len(parts) >= 2:
                key = os.path.basename(parts[1]).rsplit('.', 1)[0]
                ours[key] = parts
    rows = []
    with open(os.path.join(out, 'keys.txt'), errors='replace') as fh:
        for line in fh:
            parts = line.rstrip('\n').split('\t')
            if len(parts) < 4:
                continue
            key, path, page, refst = parts[:4]
            if refst != 'ok':
                rows.append((key, path, page, refst, '', '', '', '', '', '', ''))
                continue
            o = ours.get(key)
            if o is None:
                rows.append((key, path, page, 'missing', '', '', '', '', '', '', ''))
                continue
            if o[0] != 'OK':
                rows.append((key, path, page, o[0].lower(), '', '', '', '', '', o[2] if len(o) > 2 else '', ''))
                continue
            _, pam, scene_ms, raster_ms, cmds, notes = (o + [''] * 6)[:6]
            ref = os.path.join(out, 'ref', key + '.png')
            mine = os.path.join(out, 'ours', key + '.png')
            magick(pam, '-background', 'white', '-alpha', 'remove', '-alpha', 'off', mine)
            sr, so = size_of(ref), size_of(mine)
            if not sr or not so:
                rows.append((key, path, page, 'noimage', '', '', scene_ms, raster_ms, cmds, notes, ''))
                continue
            w, h = min(sr[0], so[0]), min(sr[1], so[1])
            crop = '%dx%d+0+0' % (w, h)
            a = ['(', ref, '-crop', crop, '+repage', '-alpha', 'off', ')']
            b = ['(', mine, '-crop', crop, '+repage', '-alpha', 'off', ')']
            _, rm = magick('compare', '-metric', 'RMSE', *a, *b, 'null:')
            _, ae = magick('compare', '-metric', 'AE', '-fuzz', '15%', *a, *b, 'null:')
            try:
                rmse = float(rm.split('(')[1].split(')')[0])
            except (IndexError, ValueError):
                rmse = -1
            try:
                aep = 100.0 * float(ae.split()[0]) / (w * h)
            except (IndexError, ValueError):
                aep = -1
            status = 'ok' if aep >= 0 and aep <= 3 else 'diff'
            if abs(sr[0] - so[0]) > 1 or abs(sr[1] - so[1]) > 1:
                status = 'size'
            cae = ''
            cref = os.path.join(out, 'cairo', key + '.png')
            if os.path.exists(cref):
                c = ['(', cref, '-crop', crop, '+repage', '-alpha', 'off', ')']
                _, ae2 = magick('compare', '-metric', 'AE', '-fuzz', '15%', *c, *b, 'null:')
                try:
                    cae = '%.3f' % (100.0 * float(ae2.split()[0]) / (w * h))
                except (IndexError, ValueError):
                    cae = ''
            rows.append((key, path, page, status, '%.4f' % rmse, '%.3f' % aep, scene_ms, raster_ms, cmds, notes, cae))
    with open(os.path.join(out, 'results.tsv'), 'w') as fh:
        fh.write('key\tpath\tpage\tstatus\trmse\tae%\tscene_ms\traster_ms\tcmds\tnotes\tcairo_ae%\n')
        for r in rows:
            fh.write('\t'.join(r) + '\n')
    aes = sorted(float(r[5]) for r in rows if r[5] not in ('', '-1.000'))
    n = len(aes)
    print('pages compared: %d (of %d rows)' % (n, len(rows)))
    for lim in (0.5, 1, 2, 3, 5, 10, 25):
        print('  AE <= %5.1f%%: %4d (%.0f%%)' % (lim, sum(1 for x in aes if x <= lim), 100.0 * sum(1 for x in aes if x <= lim) / max(1, n)))
    if n:
        print('  median AE %.3f%%, p90 %.3f%%, max %.3f%%' % (aes[n // 2], aes[min(n - 1, int(n * 0.9))], aes[-1]))
    caes = sorted(float(r[10]) for r in rows if len(r) > 10 and r[10] != '')
    if caes:
        m = len(caes)
        print('against pdftocairo: %d pages' % m)
        for lim in (0.5, 1, 2, 3, 5):
            print('  AE <= %5.1f%%: %4d (%.0f%%)' % (lim, sum(1 for x in caes if x <= lim), 100.0 * sum(1 for x in caes if x <= lim) / m))
        print('  median AE %.3f%%, p90 %.3f%%, max %.3f%%' % (caes[m // 2], caes[min(m - 1, int(m * 0.9))], caes[-1]))
    st = {}
    for r in rows:
        st[r[3]] = st.get(r[3], 0) + 1
    print('  status: ' + ', '.join('%s=%d' % kv for kv in sorted(st.items())))
    sc = sorted(int(r[6]) for r in rows if r[6].isdigit())
    ra = sorted(int(r[7]) for r in rows if r[7].isdigit())
    if sc:
        print('  scene ms: median %d, p90 %d, max %d' % (sc[len(sc) // 2], sc[int(len(sc) * 0.9)], sc[-1]))
    if ra:
        print('  raster ms: median %d, p90 %d, max %d' % (ra[len(ra) // 2], ra[int(len(ra) * 0.9)], ra[-1]))
    worst = sorted((r for r in rows if r[5] not in ('',)), key=lambda r: -float(r[5]))[:20]
    print('worst pages:')
    for r in worst:
        print('  %s ae=%s%% cairo=%s%% rmse=%s %s p%s  %s' % (r[0], r[5], r[10], r[4], os.path.basename(r[1]), r[2], r[9][:120]))
    cworst = sorted((r for r in rows if len(r) > 10 and r[10] != ''), key=lambda r: -float(r[10]))[:20]
    print('worst pages against pdftocairo:')
    for r in cworst:
        print('  %s cairo=%s%% ae=%s%% %s p%s  %s' % (r[0], r[10], r[5], os.path.basename(r[1]), r[2], r[9][:120]))


if __name__ == '__main__':
    main(sys.argv[1])
