import os, pty, time, select, fcntl, termios, struct, sys
def rss(pid):
    try: return int([l for l in open("/proc/%d/status"%pid) if l.startswith("VmRSS")][0].split()[1])//1024
    except Exception: return -1
def drain(fd, secs):
    t=time.monotonic()
    while time.monotonic()-t < secs:
        r,_,_=select.select([fd],[],[],0.05)
        if r:
            try:
                if not os.read(fd,65536): return
            except OSError: return
pid, fd = pty.fork()
if pid == 0:
    os.execv("./cmedit", ["./cmedit", sys.argv[1]])
fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", 50, 200, 1600, 800))
drain(fd, 4); print("after open:      %4d MB" % rss(pid))
os.write(fd, b"\x17")           # Ctrl+W = close file
drain(fd, 3); print("after close:     %4d MB" % rss(pid))
for _ in range(50):             # type a bit in the now-empty buffer
    os.write(fd, b"hello world "); time.sleep(0.01)
drain(fd, 3); print("after typing:    %4d MB" % rss(pid))
# The editor runs one major collection after ~30s of quiet (App.idleGcDelayUs),
# which is what returns the pages; sample either side of it.
drain(fd, 20); print("after 20s idle:  %4d MB" % rss(pid))
drain(fd, 18); print("after 38s idle:  %4d MB" % rss(pid), "  <- idle collection has fired")
os.write(fd, b"\x11"); time.sleep(0.3)
try: os.kill(pid, 9)
except ProcessLookupError: pass
