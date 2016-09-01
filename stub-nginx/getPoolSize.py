import gdb

class ParseHttpBreakpoint(gdb.Breakpoint):
    def stop(self):
        sys.stderr.write("Handling breakpoint\n")
        pool_size = gdb.parse_and_eval("r->connection->listening->pool_size")
        pool_size_addr = gdb.parse_and_eval("&r->connection->listening->pool_size")
        gdb.write("pool_size %s @ %s\n" % (pool_size, pool_size_addr))
        open("/tmp/foo", "w")
        gdb.execute("quit")
        return False	# continue

sys.stderr.write("Starting up\n")
ParseHttpBreakpoint("ngx_http_parse_request_line", temporary=False)
