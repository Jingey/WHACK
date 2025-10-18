class Wire:
    def __init__(self, funcs=None):
        self.funcs: list = funcs if funcs is not None else []

    def enable(self):
        for func in self.funcs:
            func()

        self.funcs = []

    def enlist(self, func):
        self.funcs.append(func)

    def delist(self, func):
        self.funcs.remove(func)


class Bus:
    def __init__(self, data: int = 0):
        self.data = data

    def set_data(self, data):
        tmp = self.data
        self.data = data

        return tmp

    def read_data(self):
        return self.data


# for hooking together multiple buses
# works as a multiplexer/demultiplexer
class BusSwitch(Bus):
    def __init__(self, buses: list[Bus], switch_bus: Bus):
        self.buses = buses
        self.switch_bus = switch_bus

    def set_data(self, data):
        return self.buses[self.switch_bus.read_data()].set_data(data)

    def read_data(self):
        return self.buses[self.switch_bus.read_data()].read_data()


# used to buffer the CU writing to the main bus so that it doesn't overide data
class BusCopier:
    def __init__(self, bus_from: Bus, bus_to: Bus, enable: Wire):
        self.bus_from = bus_from
        self.bus_to = bus_to
        self.enable = enable
        self.enable.enlist(self.execute)

    def execute(self):
        self.bus_to.set_data(self.bus_from.read_data())


class Register:
    def __init__(self, in_bus: Bus, out_bus: Bus, read: Wire, write: Wire):
        self.in_bus = in_bus
        self.out_bus = out_bus
        self.data = []
        read.enlist(lambda _: self.read)
        write.enlist(lambda _: self.write)

    def read(self):
        self.data = self.in_bus.data

    def write(self):
        self.out_bus.data = self.data

    def clear(self):
        self.data = 0
