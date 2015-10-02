./main.native -cmp true -debug 1 -print box -dot t.dot -log 2 parabola_i1.lts && dot -Tps t.dot >parabola_i1_l2.ps
./main.native -cmp true -debug 1 -print box -dot t.dot -log 2 parabola_i2.lts && dot -Tps t.dot >parabola_i2_l2.ps
./main.native -cmp true -debug 1 -print box -dot t.dot -log 2 cubic_i1.lts && dot -Tps t.dot >cubic_i1_l2.ps
./main.native -cmp true -debug 1 -print box -dot t.dot -log 2 cubic_i2.lts && dot -Tps t.dot >cubic_i2_l2.ps
./main.native -cmp true -debug 1 -print box -dot t.dot -log 2 exp_div.lts && dot -Tps t.dot >exp_div_l2.ps
./main.native -cmp true -debug 1 -print box -dot t.dot -log 2 oscillator_i0.lts && dot -Tps t.dot >oscillator_i0_l2.ps
./main.native -cmp true -debug 1 -print box -dot t.dot -log 2 oscillator_i1.lts && dot -Tps t.dot >oscillator_i1_l2.ps
./main.native -cmp true -debug 1 -print box -dot t.dot -log 2 inv_pendulum.lts && dot -Tps t.dot >inv_pendulum_l2.ps
./main.native -cmp true -debug 1 -print box -dot t.dot -eps 45 -log 2 thermostat.lts && dot -Tps t.dot >thermostat2.ps
./main.native -cmp true -debug 1 -print box -dot t.dot -log 2 -widening 3 0 -quadrant true oscillation2_16.lts && dot -Tps t.dot >oscillation2_16_l2w3.ps
./main.native -cmp true -debug 1 -print box -dot t.dot -log 3 -widening 3 0 -quadrant true oscillation2_16.lts && dot -Tps t.dot >oscillation2_16_l3w3.ps
./main.native -cmp true -debug 1 -print box -dot t.dot -log 2 -widening 3 0 -quadrant true oscillation2_32.lts && dot -Tps t.dot >oscillation2_32_l2w3.ps
./main.native -cmp true -debug 1 -print box -dot t.dot -log 3 -widening 2 0 -quadrant true oscillation2_32.lts && dot -Tps t.dot >oscillation2_32_l3w2.ps
./main.native -cmp true -debug 1 -print box -dot t.dot -log 3 -widening 3 0 -quadrant true oscillation2_32.lts && dot -Tps t.dot >oscillation2_32_l3w3.ps

./main.native -cmp true -debug 1 -print box -dot t.dot -log 4 parabola_i1.lts && dot -Tps t.dot >parabola_i1_l4.ps
./main.native -cmp true -debug 1 -print box -dot t.dot -log 4 parabola_i2.lts && dot -Tps t.dot >parabola_i2_l4.ps
./main.native -cmp true -debug 1 -print box -dot t.dot -log 4 cubic_i1.lts && dot -Tps t.dot >cubic_i1_l4.ps
./main.native -cmp true -debug 1 -print box -dot t.dot -log 4 cubic_i2.lts && dot -Tps t.dot >cubic_i2_l4.ps
./main.native -cmp true -debug 1 -print box -dot t.dot -log 4 exp_div.lts && dot -Tps t.dot >exp_div_l4.ps
./main.native -cmp true -debug 1 -print box -dot t.dot -log 4 oscillator_i0.lts && dot -Tps t.dot >oscillator_i0_l4.ps
./main.native -cmp true -debug 1 -print box -dot t.dot -log 4 oscillator_i1.lts && dot -Tps t.dot >oscillator_i1_l4.ps
./main.native -cmp true -debug 1 -print box -dot t.dot -log 4 inv_pendulum.lts && dot -Tps t.dot >inv_pendulum_l4.ps
./main.native -cmp true -debug 1 -print box -dot t.dot -eps 45 -log 4 thermostat.lts && dot -Tps t.dot >thermostat2.ps
./main.native -cmp true -debug 1 -print box -dot t.dot -log 4 -widening 3 0 -quadrant true oscillation2_16.lts && dot -Tps t.dot >oscillation2_16_l4w3.ps
./main.native -cmp true -debug 1 -print box -dot t.dot -log 4 -widening 3 0 -quadrant true oscillation2_32.lts && dot -Tps t.dot >oscillation2_32_l4w3.ps

# ***************************

./main.native -debug 1 -print box -dot t.dot -log 1 parabola_i0.lts && dot -Tps t.dot >parabola_i0_l1.ps
./main.native -debug 1 -print box -dot t.dot -log 2 parabola_i0.lts && dot -Tps t.dot >parabola_i0_l2.ps
./main.native -debug 1 -print box -dot t.dot -log 4 parabola_i0.lts && dot -Tps t.dot >parabola_i0_l4.ps
./main.native -debug 1 -print box -dot t.dot -log 1 parabola_i1.lts && dot -Tps t.dot >parabola_i1_l1.ps
./main.native -debug 1 -print box -dot t.dot -log 2 parabola_i1.lts && dot -Tps t.dot >parabola_i1_l2.ps
./main.native -debug 1 -print box -dot t.dot -log 4 parabola_i1.lts && dot -Tps t.dot >parabola_i1_l4.ps
./main.native -debug 1 -print box -dot t.dot -log 1 parabola_i2.lts && dot -Tps t.dot >parabola_i2_l1.ps
./main.native -debug 1 -print box -dot t.dot -log 2 parabola_i2.lts && dot -Tps t.dot >parabola_i2_l2.ps
./main.native -debug 1 -print box -dot t.dot -log 4 parabola_i2.lts && dot -Tps t.dot >parabola_i2_l4.ps

./main.native -debug 1 -print box -dot t.dot -log 1 cubic_i0.lts && dot -Tps t.dot >cubic_i0_l1.ps
./main.native -debug 1 -print box -dot t.dot -log 2 cubic_i0.lts && dot -Tps t.dot >cubic_i0_l2.ps
./main.native -debug 1 -print box -dot t.dot -log 4 cubic_i0.lts && dot -Tps t.dot >cubic_i0_l4.ps
./main.native -debug 1 -print box -dot t.dot -log 1 cubic_i1.lts && dot -Tps t.dot >cubic_i1_l1.ps
./main.native -debug 1 -print box -dot t.dot -log 2 cubic_i1.lts && dot -Tps t.dot >cubic_i1_l2.ps
./main.native -debug 1 -print box -dot t.dot -log 4 cubic_i1.lts && dot -Tps t.dot >cubic_i1_l4.ps
./main.native -debug 1 -print box -dot t.dot -log 1 cubic_i2.lts && dot -Tps t.dot >cubic_i2_l1.ps
./main.native -debug 1 -print box -dot t.dot -log 2 cubic_i2.lts && dot -Tps t.dot >cubic_i2_l2.ps
./main.native -debug 1 -print box -dot t.dot -log 4 cubic_i2.lts && dot -Tps t.dot >cubic_i2_l4.ps

./main.native -debug 1 -print box -dot t.dot -log 1 exp_div.lts && dot -Tps t.dot >exp_div_l1.ps
./main.native -debug 1 -print box -dot t.dot -log 2 exp_div.lts && dot -Tps t.dot >exp_div_l2.ps
./main.native -debug 1 -print box -dot t.dot -log 3 exp_div.lts && dot -Tps t.dot >exp_div_l3.ps

./main.native -debug 1 -print box -dot t.dot -log 1 oscillator_i0.lts && dot -Tps t.dot >oscillator_i0_l1.ps
./main.native -debug 1 -print box -dot t.dot -log 2 oscillator_i0.lts && dot -Tps t.dot >oscillator_i0_l2.ps
./main.native -debug 1 -print box -dot t.dot -log 4 oscillator_i0.lts && dot -Tps t.dot >oscillator_i0_l4.ps
./main.native -debug 1 -print box -dot t.dot -log 1 oscillator_i1.lts && dot -Tps t.dot >oscillator_i1_l1.ps
./main.native -debug 1 -print box -dot t.dot -log 2 oscillator_i1.lts && dot -Tps t.dot >oscillator_i1_l2.ps
./main.native -debug 1 -print box -dot t.dot -log 4 oscillator_i1.lts && dot -Tps t.dot >oscillator_i1_l4.ps

./main.native -debug 1 -print box -dot t.dot -log 0 inv_pendulum.lts && dot -Tps t.dot >inv_pendulum_l0.ps
./main.native -debug 1 -print box -dot t.dot -log 1 inv_pendulum.lts && dot -Tps t.dot >inv_pendulum_l1.ps
./main.native -debug 1 -print box -dot t.dot -log 2 inv_pendulum.lts && dot -Tps t.dot >inv_pendulum_l2.ps
./main.native -debug 1 -print box -dot t.dot -log 3 inv_pendulum.lts && dot -Tps t.dot >inv_pendulum_l3.ps
./main.native -debug 1 -print box -dot t.dot -log 4 inv_pendulum.lts && dot -Tps t.dot >inv_pendulum_l4.ps

./main.native -debug 1 -print boxpoly -dot t.dot -eps 45 -log 1 thermostat.lts && dot -Tps t.dot >thermostat1.ps
./main.native -debug 1 -print boxpoly -dot t.dot -eps 45 -log 2 thermostat.lts && dot -Tps t.dot >thermostat2.ps
./main.native -debug 1 -print boxpoly -dot t.dot -eps 45 -log 3 thermostat.lts && dot -Tps t.dot >thermostat3.ps
./main.native -debug 1 -print boxpoly -dot t.dot -eps 45 -log 4 thermostat.lts && dot -Tps t.dot >thermostat4.ps
./main.native -debug 1 -print boxpoly -dot t.dot -eps 45 -log 5 thermostat.lts && dot -Tps t.dot >thermostat5.ps
./main.native -debug 1 -print boxpoly -dot t.dot -eps 45 -log 6 thermostat.lts && dot -Tps t.dot >thermostat6.ps
./main.native -debug 1 -print boxpoly -dot t.dot -eps 45 -log 8 thermostat.lts && dot -Tps t.dot >thermostat8.ps

./main.native -debug 1 -print box -dot t.dot -log 2 -widening 3 0 -quadrant true oscillation2_16.lts && dot -Tps t.dot >oscillation2_16_l2w3.ps
./main.native -debug 1 -print box -dot t.dot -log 2 -widening 3 0 -quadrant true oscillation2_32.lts && dot -Tps t.dot >oscillation2_32_l2w3.ps
./main.native -debug 1 -print box -dot t.dot -log 3 -widening 3 0 -quadrant true oscillation2_16.lts && dot -Tps t.dot >oscillation2_16_l3w3.ps
./main.native -debug 1 -print box -dot t.dot -log 3 -widening 3 0 -quadrant true oscillation2_32.lts && dot -Tps t.dot >oscillation2_32_l3w3.ps
./main.native -debug 1 -print box -dot t.dot -log 4 -widening 3 0 -quadrant true oscillation2_16.lts && dot -Tps t.dot >oscillation2_16_l4w3.ps
./main.native -debug 1 -print box -dot t.dot -log 4 -widening 3 0 -quadrant true oscillation2_32.lts && dot -Tps t.dot >oscillation2_32_l4w3.ps
