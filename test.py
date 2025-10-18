from alu import Alu
from wires import Bus, Wire

p_bus = Bus()
q_bus = Bus()
func_bus = Bus()
enable = Wire()
out_bus = Bus()

alu = Alu(p_bus, q_bus, func_bus, enable, out_bus)

p_bus.set_data(102)
q_bus.set_data(212)

func_bus.set_data(0)

enable.enable()

print(out_bus.data)
