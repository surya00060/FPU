#!/bin/sh

# 
# Vivado(TM)
# runme.sh: a Vivado-generated Runs Script for UNIX
# Copyright 1986-2016 Xilinx, Inc. All Rights Reserved.
# 

if [ -z "$PATH" ]; then
  PATH=/tools/Vivado/SDK/2016.1/bin:/tools/Vivado/Vivado/2016.1/ids_lite/ISE/bin/lin64:/tools/Vivado/Vivado/2016.1/bin
else
  PATH=/tools/Vivado/SDK/2016.1/bin:/tools/Vivado/Vivado/2016.1/ids_lite/ISE/bin/lin64:/tools/Vivado/Vivado/2016.1/bin:$PATH
fi
export PATH

if [ -z "$LD_LIBRARY_PATH" ]; then
  LD_LIBRARY_PATH=/tools/Vivado/Vivado/2016.1/ids_lite/ISE/lib/lin64
else
  LD_LIBRARY_PATH=/tools/Vivado/Vivado/2016.1/ids_lite/ISE/lib/lin64:$LD_LIBRARY_PATH
fi
export LD_LIBRARY_PATH

HD_PWD='/home/surya/Desktop/FPU/fpga/mkfpu_fm_add_sub_pipe_64_opt/mkfpu_fm_add_sub_pipe_64_opt.runs/core_synth_1'
cd "$HD_PWD"

HD_LOG=runme.log
/bin/touch $HD_LOG

ISEStep="./ISEWrap.sh"
EAStep()
{
     $ISEStep $HD_LOG "$@" >> $HD_LOG 2>&1
     if [ $? -ne 0 ]
     then
         exit
     fi
}

EAStep vivado -log mkfpu_fm_add_sub_pipe_64_opt.vds -m64 -mode batch -messageDb vivado.pb -notrace -source mkfpu_fm_add_sub_pipe_64_opt.tcl
