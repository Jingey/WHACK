class Wire:
    def __init__(self, funcs=None):
        self.funcs: list = funcs if funcs is not None else []

    def call_all(self, data):
        for func in self.funcs:
            func(data)

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


class Register:
    def __init__(self, inBus: Bus, outBus: Bus, read: Wire, write: Wire):
        self.inBus = inBus
        self.outBus = outBus
        self.data = []
        read.enlist(lambda _: self.read)
        write.enlist(lambda _: self.write)

    def read(self):
        self.data = self.inBus.data

    def write(self):
        self.outBus.data = self.data

    def clear(self):
        self.data = 0
