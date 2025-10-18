import logging


class Logger:
    def __init__(self):
        with open("CPU.log", "w") as f:
            f.write("")
        logging.basicConfig(filename="CPU.log", level=logging.INFO)

        self.base_logger = logging.getLogger()

    def log_clock(self):
        self.base_logger.warning("Next tick started")

    def log_wire_notifying(self, wire):
        self.base_logger.info(f"Wire {wire.name} is enabled")

    def log_alu_calculating(self, left, right, func):
        self.base_logger.info(
            f"ALU calculating: left = {left}, right={right}, func={func}"
        )

    def log_alu_result(self, result, ccr):
        self.base_logger.info(f"ALU finished: result = {result}, ccr = {bin(ccr)}")

    def log_bus_written(self, bus):
        self.base_logger.info(f"Bus {bus.name} is written to with {bus.data}")

    def log_bus_read(self, bus):
        self.base_logger.info(f"Bus {bus.name} is read from with {bus.data}")

    def log_register_loaded(self, reg):
        self.base_logger.info(f"Reg {reg.name} is written to with {reg.data}")

    def log_register_outputs(self, reg):
        self.base_logger.info(f"Reg {reg.name} outputed {reg.data}")

    def log_bus_copied(self, copy):
        self.base_logger.info(
            f"Bus {copy.bus_from.name} is copied to {copy.bus_to.name} with {copy.bus_from.data}"
        )

    def log_ms_load(self, addr, data):
        self.base_logger.info(f"Main store read {data} from address {addr}")

    def log_ms_save(self, addr, data):
        self.base_logger.info(f"Main store saved {data} to address {addr}")


log: Logger = Logger()
