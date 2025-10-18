from .wires import Wire, Bus


class MainStorage:
    """
    0 = write, 1 = read.
    """

    def __init__(self, enable: Wire, address_bus: Bus, data_bus: Bus, rw_bus: Bus):
        self.enable = enable
        self.address_bus = address_bus
        self.data_bus = data_bus
        self.rw_bus = rw_bus
        self.mem = [0] * (2 ** 16)
        self.enable.enlist(lambda data: self.execute())

    def execute(self):
        if self.rw_bus.read_data() == 1:
            self.read()
        else:
            self.write()

    def read(self):
        self.data_bus.set_data(self.mem[self.address_bus.read_data()])

    def write(self):
        self.mem[self.address_bus.read_data()] = self.data_bus.read_data()

    def load(self, data):
        self.mem = data
