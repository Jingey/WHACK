from logs import log


class Wire:
    def __init__(self, name):
        self.funcs = []
        self.late_funcs = []
        self.name = name

    def enable(self):
        log.log_wire_notifying(self)
        for func in self.funcs:
            func()

        for func in self.late_funcs:
            func()

    def enlist(self, func):
        self.funcs.append(func)

    def delist(self, func):
        self.funcs.remove(func)

    def late_enlist(self, func):
        self.late_funcs.append(func)

    def late_delist(self, func):
        self.late_funcs.remove(func)


class Bus:
    def __init__(self, name, data: int = 0):
        self.data = data
        self.name = name

    def set_data(self, data):
        tmp = self.data
        self.data = data
        log.log_bus_written(self)

        return tmp

    def read_data(self):
        log.log_bus_read(self)
        return self.data


# used to buffer the CU writing to the main bus so that it doesn't overide data
class BusCopier:
    def __init__(self, bus_from: Bus, bus_to: Bus, enable: Wire):
        self.bus_from = bus_from
        self.bus_to = bus_to
        self.enable = enable
        self.enable.enlist(self.execute)

    def execute(self):
        log.log_bus_copied(self)
        self.bus_to.set_data(self.bus_from.read_data())


class Register:
    def __init__(self, name, in_bus: Bus, out_bus: Bus, read: Wire, write: Wire):
        self.in_bus = in_bus
        self.out_bus = out_bus
        self.data = 0
        self.name = name
        read.enlist(self.read)
        write.enlist(self.write)

    def read(self):
        self.data = self.in_bus.read_data()
        log.log_register_loaded(self)

    def write(self):
        log.log_register_outputs(self)
        self.out_bus.set_data(self.data)

    def clear(self):
        self.data = 0


class Incrementer:
    def __init__(self, reg: Register, inc: Wire):
        self.reg = reg
        self.inc = inc
        self.inc.enlist(self.increment)

    def increment(self):
        self.reg.data += 1


class FeStatus:
    def __init__(self, clock: Wire):
        self.data = 0
        self.clock = clock
        self.clock.late_enlist(self.increment)

    def increment(self):
        self.data += 1
        self.data %= 4
