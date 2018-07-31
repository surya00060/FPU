proc start_step { step } {
  set stopFile ".stop.rst"
  if {[file isfile .stop.rst]} {
    puts ""
    puts "*** Halting run - EA reset detected ***"
    puts ""
    puts ""
    return -code error
  }
  set beginFile ".$step.begin.rst"
  set platform "$::tcl_platform(platform)"
  set user "$::tcl_platform(user)"
  set pid [pid]
  set host ""
  if { [string equal $platform unix] } {
    if { [info exist ::env(HOSTNAME)] } {
      set host $::env(HOSTNAME)
    }
  } else {
    if { [info exist ::env(COMPUTERNAME)] } {
      set host $::env(COMPUTERNAME)
    }
  }
  set ch [open $beginFile w]
  puts $ch "<?xml version=\"1.0\"?>"
  puts $ch "<ProcessHandle Version=\"1\" Minor=\"0\">"
  puts $ch "    <Process Command=\".planAhead.\" Owner=\"$user\" Host=\"$host\" Pid=\"$pid\">"
  puts $ch "    </Process>"
  puts $ch "</ProcessHandle>"
  close $ch
}

proc end_step { step } {
  set endFile ".$step.end.rst"
  set ch [open $endFile w]
  close $ch
}

proc step_failed { step } {
  set endFile ".$step.error.rst"
  set ch [open $endFile w]
  close $ch
}

set_msg_config -id {HDL 9-1061} -limit 100000
set_msg_config -id {HDL 9-1654} -limit 100000

start_step place_design
set rc [catch {
  create_msg_db place_design.pb
  open_checkpoint mkfpu_fm_add_sub_pipe_64_opt.dcp
  set_property webtalk.parent_dir /home/surya/Desktop/FPU/fpga/mkfpu_fm_add_sub_pipe_64/mkfpu_fm_add_sub_pipe_64.cache/wt [current_project]
  implement_debug_core 
  place_design 
  write_checkpoint -force mkfpu_fm_add_sub_pipe_64_placed.dcp
  report_io -file mkfpu_fm_add_sub_pipe_64_io_placed.rpt
  report_utilization -file mkfpu_fm_add_sub_pipe_64_utilization_placed.rpt -pb mkfpu_fm_add_sub_pipe_64_utilization_placed.pb
  report_control_sets -verbose -file mkfpu_fm_add_sub_pipe_64_control_sets_placed.rpt
  close_msg_db -file place_design.pb
} RESULT]
if {$rc} {
  step_failed place_design
  return -code error $RESULT
} else {
  end_step place_design
}

start_step route_design
set rc [catch {
  create_msg_db route_design.pb
  route_design 
  write_checkpoint -force mkfpu_fm_add_sub_pipe_64_routed.dcp
  report_drc -file mkfpu_fm_add_sub_pipe_64_drc_routed.rpt -pb mkfpu_fm_add_sub_pipe_64_drc_routed.pb
  report_timing_summary -warn_on_violation -max_paths 10 -file mkfpu_fm_add_sub_pipe_64_timing_summary_routed.rpt -rpx mkfpu_fm_add_sub_pipe_64_timing_summary_routed.rpx
  report_power -file mkfpu_fm_add_sub_pipe_64_power_routed.rpt -pb mkfpu_fm_add_sub_pipe_64_power_summary_routed.pb -rpx mkfpu_fm_add_sub_pipe_64_power_routed.rpx
  report_route_status -file mkfpu_fm_add_sub_pipe_64_route_status.rpt -pb mkfpu_fm_add_sub_pipe_64_route_status.pb
  report_clock_utilization -file mkfpu_fm_add_sub_pipe_64_clock_utilization_routed.rpt
  close_msg_db -file route_design.pb
} RESULT]
if {$rc} {
  step_failed route_design
  return -code error $RESULT
} else {
  end_step route_design
}

