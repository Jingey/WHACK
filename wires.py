class Wire:
    def __init__(self, funcs=None):
        self.funcs: list = funcs if funcs is not None else []

    def callAll(self, data):
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

    def set(self, data):
        tmp = self.data
        self.data = data

        return tmp


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
