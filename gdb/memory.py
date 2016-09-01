import copy
import gdb

def debug(s):
    if False:
        gdb.write(s)

class AddressClass():
    def __init__(self, name, descr):
        self.name = name
        self.descr = descr

class AddressClassResolver():
    classes = {
        gdb.SYMBOL_LOC_UNDEF : AddressClass('SYMBOL_LOC_UNDEF',
                                            "If this is returned by address class, it indicates an error either in the symbol information or in gdb's handling of symbols."),
        gdb.SYMBOL_LOC_CONST : AddressClass('SYMBOL_LOC_CONST', "Value is constant int"),
        gdb.SYMBOL_LOC_STATIC : AddressClass('SYMBOL_LOC_STATIC', "Value is at a fixed address"),
        gdb.SYMBOL_LOC_REGISTER : AddressClass('SYMBOL_LOC_REGISTER', "Value is in a register"),
        gdb.SYMBOL_LOC_ARG : AddressClass('SYMBOL_LOC_ARG', "Value is an argument. This value is at the offset stored within the symbol inside the frame's argument list."),
        gdb.SYMBOL_LOC_REF_ARG : AddressClass('SYMBOL_LOC_REF_ARG', "Value address is stored in the frame's argument list. Just like LOC_ARG except that the value's address is stored at the offset, not the value itself."),
        gdb.SYMBOL_LOC_REGPARM_ADDR : AddressClass('SYMBOL_LOC_REGPARM_ADDR', "Value is a specified register. Just like LOC_REGISTER except the register holds the address of the argument instead of the argument itself."),
        gdb.SYMBOL_LOC_LOCAL : AddressClass('SYMBOL_LOC_LOCAL', "Value is a local variable."),
        gdb.SYMBOL_LOC_TYPEDEF : AddressClass('SYMBOL_LOC_TYPEDEF', "Value not used. Symbols in the domain SYMBOL_STRUCT_DOMAIN all have this class."),
        gdb.SYMBOL_LOC_BLOCK : AddressClass('SYMBOL_LOC_BLOCK', "Value is a block."),
        gdb.SYMBOL_LOC_CONST_BYTES : AddressClass('SYMBOL_LOC_CONST_BYTES', "Value is a byte-sequence."),
        gdb.SYMBOL_LOC_UNRESOLVED : AddressClass('SYMBOL_LOC_UNRESOLVED', "Value is at a fixed address, but the address of the variable has to be determined from the minimal symbol table whenever the variable is referenced."),
        gdb.SYMBOL_LOC_OPTIMIZED_OUT : AddressClass('SYMBOL_LOC_OPTIMIZED_OUT', "The value does not actually exist in the program."),
        gdb.SYMBOL_LOC_COMPUTED : AddressClass('SYMBOL_LOC_COMPUTED', "The value's address is a computed location.")
        }

    @staticmethod
    def resolve(ac):
        ret = AddressClassResolver.classes[ac]
        return ret

class MemoryU64:
    def __init__(self, val, addr):
        self.value = val
        self.addr = addr

class ExamStack(gdb.Command):
    def __init__(self):
        super(ExamStack, self).__init__("exam-stack-locals-memory", gdb.COMMAND_DATA)

    def invoke(self, arg, from_tty):
        frame = saved_frame = gdb.selected_frame()
        while True:
            gdb.write("In frame %s\n" % frame.name())
            try:
                block = frame.block()
                for sym in block:
                    ac = AddressClassResolver.resolve(sym.addr_class).name
                    addr = "n/a"
                    try:
                        addr = gdb.parse_and_eval("&%s" % sym)
                    except Exception:
                        None
                    gdb.write("\tsymbol: %s [%s|%s]: %s\n" % (sym, ac, sym.type, addr))
            except RuntimeError, msg:
                gdb.write("Exception: %s\n" % msg)
            frame = frame.older()
            if frame is None:
                break
            gdb.execute("up-silently")
        saved_frame.select()

def get_ptrs():
    frame = saved_frame = gdb.selected_frame()
    ptrs = []
    # UGH generators please
    while True:
        try:
            block = frame.block()
            for sym in block:
                typ = sym.type
                if typ.code == gdb.TYPE_CODE_PTR:
                    value = frame.read_var(sym)
                    if not value.is_optimized_out and not (value == 0):
                        ptrs.append(value)
            # XXX: structs
        except RuntimeError, msg:
            gdb.write("Exception: %s\n" % msg)
        frame = frame.older()
        if frame is None:
            break
        gdb.execute("up-silently")
    saved_frame.select()
    return ptrs

def get_u64s(start, nbytes):
    debug("get_u64s(%#x, +%s)\n" % (start, nbytes))
    vals = []
    end = start + nbytes
    for addr in xrange(start, end, 8):
        value = gdb.parse_and_eval("*(unsigned long *)%s" % addr)
        vals.append(MemoryU64(value, addr))
    return vals

def get_stack_size_to_main():
    curr_sp = gdb.parse_and_eval("$sp")
    frame = saved_frame = gdb.selected_frame()
    while frame.older() is not None:
        frame = frame.older()
    frame.select()
    main_sp = gdb.parse_and_eval("$sp")
    saved_frame.select()
    return gdb.parse_and_eval("%s - %s" % (main_sp, curr_sp))

def get_heap_ptrs_on_stack():
    stack_low = curr_sp = gdb.parse_and_eval("(unsigned long)$sp")

    # First, find all pointers in locals (both in the current frame and in
    # the callers' frames)

    ptrs = get_ptrs()

    # See up to what high an address it's safe to examine the stack
    # We look till the end of the page which includes main's frame (doubly
    # useful as we don't need to care so much about where main's frame starts).

    stacksz = get_stack_size_to_main()
    stack_high = gdb.parse_and_eval("((unsigned long)%s + %s + 4096) & (~4095)" % (curr_sp, stacksz))
    # Then, examine the aligned bytes on the stack for those pointers
    debug("high: %#x, low %#x\n" % (stack_high, stack_low))
    nbytes = stack_high - stack_low
    on_stack = get_u64s(stack_low, nbytes)

    ptrs = filter(lambda x: (x < stack_low) or (x > stack_high), ptrs)

    ret = []
    # Keep values on stack which are pointers to heap objects we've seen
    for ptr in ptrs:
        for x in on_stack:
            if x.value == ptr:
                # Return the ptr value, which has a useful type
                ret.append(MemoryU64(ptr, x.addr))
    return ret

class HeapPointersOnStack(gdb.Command):
    """
    Locate heap pointers left on the stack
    """
    def __init__(self):
        super(HeapPointersOnStack, self).__init__("heap-pointers-on-stack", gdb.COMMAND_DATA)


    def invoke(self, arg, from_tty):
        ret = get_heap_ptrs_on_stack()
        for ptr in ret:
            gdb.write("ptr: %#x@%#x [%s]\n" % (ptr.value, ptr.addr, ptr.value.type))
HeapPointersOnStack()

class Node:
    def __init__(self, value):
        self.value = value
        self.pred = []
        self.succ = []

class Found(Exception):
    def __init__(self, args):
        self.args = args
        return

class Explorer:
    def __init__(self, value, wanted):
        self.wanted = wanted
        self.seen = {}
        self.nodes = {}
        self.explore([], value)

    def explore(self, path, value):
        try:
            debug("%sexplore((%s)%s)\n" % ("  " * len(path) + "-> ", value.type, value))
        except gdb.MemoryError:
            return

        if path == []:
            path.append("(%s:%s)" % (value, value.type))

        if len(path) > 20:
            gdb.write("Exceeded maximum recursion depth\n")
            return
        if self.wanted is not None:
            if self.wanted == value:
                raise Found(path)

        typ = value.type.strip_typedefs()
        if not (typ.code == gdb.TYPE_CODE_PTR):
            raise Exception("Not a pointer type: %s" % value.type)

        if str(value) in self.nodes:
            return

        self.nodes[str(value)] = True

        try:
            value = value.dereference()
        except gdb.MemoryError:
            return
        except gdb.error:
            # Likely a void pointer deref
            return

        typ = value.type.strip_typedefs()

        debug("type after deref: %s (code: %s)\n" % (typ, typ.code))
        if typ.code == gdb.TYPE_CODE_PTR:
            debug("Chasing ptr\n")
            self.explore(path, value)
        elif typ.code == gdb.TYPE_CODE_STRUCT:
            for fld in typ.fields():
                debug("Looking at field %s\n" % fld.name)
                fldval = value[fld.name]
                fldtyp = fldval.type.strip_typedefs()
                if fldtyp.code == gdb.TYPE_CODE_STRUCT:
                    npath = copy.copy(path)
                    npath.append(". (%s:%s)" % (fld.name, fld.type))
                    self.explore(npath, fldval.address)
                elif fldtyp.code == gdb.TYPE_CODE_PTR:
                    npath = copy.copy(path)
                    npath.append("-> (%s:%s)" % (fld.name, fld.type))
                    self.explore(npath, fldval)
                else:
                    continue
        elif typ.code == gdb.TYPE_CODE_TYPEDEF:
            # We should have stripped them
            debug("Unexpected typedef\n")
            return
        elif typ.code == gdb.TYPE_CODE_UNION:
            debug("Ignoring union\n")
            return
        else:
            return

class TransitiveFind(gdb.Command):
    def __init__(self):
        super(TransitiveFind, self).__init__("transitive-find", gdb.COMMAND_DATA)

    def invoke(self, arg, from_tty):
        argv = gdb.string_to_argv(arg)
        value = gdb.parse_and_eval(argv[0])
        wanted = None
        if len(argv) > 1:
            wanted = gdb.parse_and_eval(argv[1])
        try:
            expl = Explorer(value, wanted)
        except Found, path:
            gdb.write("Found! [%s]\n" % "".join(path))
        
TransitiveFind()

class ChaseFromStack(gdb.Command):
    def __init__(self):
        super(ChaseFromStack, self).__init__("chase-from-stack", gdb.COMMAND_DATA)

    def invoke(self, arg, from_tty):
        argv = gdb.string_to_argv(arg)
        wanted = gdb.parse_and_eval(argv[0])
        ptrs_on_stack = get_heap_ptrs_on_stack()
        for ptr in ptrs_on_stack:
            try:
                Explorer(ptr.value, wanted)
            except Found, path:
                gdb.write("Found! [%s] (root was @%#x)\n" % ("".join(path), ptr.addr))
                break

ChaseFromStack()
