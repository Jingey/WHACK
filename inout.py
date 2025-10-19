from wires import Wire, Bus


class Input:
    def __init__(self, read_enable: Wire, out_bus: Bus):
        self.queue = []
        self.out_bus = out_bus
        self.read_enable = read_enable
        self.read_enable.enlist(self.write_to_bus)

    def write_to_bus(self):
        if len(self.queue) == 0:
            self.queue.extend(map(ord, input()))
            self.queue.append(0)
        self.out_bus.set_data(self.queue[0])
        del self.queue[0]


class Output:
    def __init__(self, write_enable: Wire, in_bus: Bus):
        self.in_bus = in_bus
        self.write_enable = write_enable
        self.write_enable.enlist(self.write_from_bus)

    def write_from_bus(self):
        print(chr(self.in_bus.read_data()), end="")
