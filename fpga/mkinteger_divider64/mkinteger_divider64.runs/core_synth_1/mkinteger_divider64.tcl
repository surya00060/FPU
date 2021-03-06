# 
# Synthesis run script generated by Vivado
# 

set_msg_config -id {HDL 9-1061} -limit 100000
set_msg_config -id {HDL 9-1654} -limit 100000
create_project -in_memory -part xc7a100tcsg324-1

set_param project.singleFileAddWarning.threshold 0
set_param project.compositeFile.enableAutoGeneration 0
set_param synth.vivado.isSynthRun true
set_property webtalk.parent_dir /home/surya/Desktop/FPU/fpga/mkinteger_divider64/mkinteger_divider64.cache/wt [current_project]
set_property parent.project_path /home/surya/Desktop/FPU/fpga/mkinteger_divider64/mkinteger_divider64.xpr [current_project]
set_property default_lib xil_defaultlib [current_project]
set_property target_language Verilog [current_project]
set_property include_dirs /home/surya/Desktop/FPU/verilog [current_fileset]
read_verilog -library xil_defaultlib /home/surya/Desktop/FPU/verilog/mkinteger_divider64.v
foreach dcp [get_files -quiet -all *.dcp] {
  set_property used_in_implementation false $dcp
}
read_xdc /home/surya/Desktop/FPU/src/tcl/constraints.xdc
set_property used_in_implementation false [get_files /home/surya/Desktop/FPU/src/tcl/constraints.xdc]


synth_design -top mkinteger_divider64 -part xc7a100tcsg324-1 -flatten_hierarchy none


write_checkpoint -force -noxdef mkinteger_divider64.dcp

catch { report_utilization -file mkinteger_divider64_utilization_synth.rpt -pb mkinteger_divider64_utilization_synth.pb }
