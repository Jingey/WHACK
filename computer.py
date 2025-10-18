from wires import Wire


class Computer:
    def __init__(self, clock: Wire, halt: Wire):
        self.clock = clock
        self.finished = False
        self.halt = halt
        halt.enlist(self.halted)

    def halted(self):
        self.finished = True

    def run_cycle(self):
        if not self.finished:
            self.clock.enable()

    def run(self):
        while not self.finished:
            self.clock.enable()
