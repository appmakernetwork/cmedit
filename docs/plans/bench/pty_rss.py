import os, pty, time, select, fcntl, termios, struct, sys
pid, fd = pty.fork()
if pid == 0:
    os.execv("./cmedit", ["./cmedit", sys.argv[1]])
fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", 50, 200, 1600, 800))
t0 = time.monotonic()
samples = []
next_s = 1.0
while time.monotonic() - t0 < 25:
    r,_,_ = select.select([fd],[],[],0.05)
    if r:
        try: os.read(fd, 65536)
        except OSError: break
    if time.monotonic() - t0 >= next_s:
        try:
            rss = int([l for l in open("/proc/%d/status"%pid) if l.startswith("VmRSS")][0].split()[1])
            samples.append((round(time.monotonic()-t0,1), rss//1024))
        except Exception: break
        next_s += 2.0
print(sys.argv[1], "RSS MB over time:", samples)
os.write(fd, b"\x11"); time.sleep(0.3)
try: os.kill(pid, 9)
except ProcessLookupError: pass
