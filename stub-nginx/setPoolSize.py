import gdb

class SetPoolSize(gdb.Command):
    def __init__(self):
        super (SetPoolSize, self).__init__ ("set-pool-size", gdb.COMMAND_DATA)

    def invoke (self, arg, from_tty):
        argv = gdb.string_to_argv(arg)
        addr = argv[0]
        nval = argv[1]
        gdb.parse_and_eval("((size_t *)%s)[0] = %s" % (addr, nval))
        sys.stderr.write("Set pool at %s to %s" % (addr, nval))

SetPoolSize()
