import os, pty, time, select, fcntl, termios, struct, sys, resource

def run(args, keys_after_first_frame=b"", settle=1.5):
    pid, fd = pty.fork()
    if pid == 0:
        os.execv(args[0], args)
    fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", 50, 200, 1600, 800))
    t0 = time.monotonic()
    first = None
    total = 0
    deadline = t0 + settle
    while time.monotonic() < deadline:
        r, _, _ = select.select([fd], [], [], 0.05)
        if r:
            try:
                data = os.read(fd, 65536)
            except OSError:
                break
            if not data:
                break
            if first is None and len(data) > 0:
                first = time.monotonic() - t0
            total += len(data)
    rss = None
    try:
        rss = int([l for l in open("/proc/%d/status" % pid) if l.startswith("VmRSS")][0].split()[1])
    except Exception:
        pass
    if keys_after_first_frame:
        os.write(fd, keys_after_first_frame)
        time.sleep(0.3)
        try:
            while select.select([fd], [], [], 0.05)[0]:
                d = os.read(fd, 65536)
                if not d: break
                total += len(d)
        except OSError:
            pass
    os.write(fd, b"\x11")   # Ctrl+Q
    time.sleep(0.4)
    try:
        os.kill(pid, 15)
    except ProcessLookupError:
        pass
    try:
        os.waitpid(pid, os.WNOHANG)
    except ChildProcessError:
        pass
    os.close(fd)
    return first, total, rss

for label, argv in [("no file", ["./cmedit"]),
                    ("49MB text", ["./cmedit", sys.argv[1]]),
                    ("32MB csv", ["./cmedit", sys.argv[2]]),
                    ("603KB jpeg", ["./cmedit", "Van_Gogh_-_Starry_Night_-_Google_Art_Project.jpg"])]:
    try:
        first, total, rss = run(argv, settle=6.0)
        print("%-12s first output %s, %d bytes in 6s, RSS %s MB"
              % (label,
                 ("%.0f ms" % (first*1000)) if first else "none",
                 total, ("%.0f" % (rss/1024)) if rss else "?"))
    except Exception as e:
        print("%-12s FAILED: %r" % (label, e))
