from alu import Alu
from wires import Register, Bus, Wire

p_bus = Bus()
q_bus = Bus()
func_bus = Bus()
enable = Wire()
out_bus = Bus()

alu = Alu(p_bus, q_bus, func_bus, enable, out_bus)

p_bus.set(102)
q_bus.set(212)

func_bus.set(0)

enable.call_all(1)

print(out_bus.data)
