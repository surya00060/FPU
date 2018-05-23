## Logical Equalence Checking

### Pre LEC Setup

#### Cadence Conformal LEC
|File ($SHAKTI_HOME/asic_flow/lec/conformal_lec/)                        | Update|
|--------------------------------| -------- |
|./lib_list       |List library files including the path|
|./net_list         |List net-list files including the path|
|./rtl_list        |List RTL files including the path|
|./design.do |Find and replace ***"design"*** with top module name

### Fire LEC

From $SHAKTI_HOME/asic_flow/lec/ folder invoke using ***lec*** target of the Makefile.

### Note

* This script does not have DFT feature inclusion.