#Copyright (c) 2013, IIT Madras All rights reserved.
#
#Redistribution and use in source and binary forms, with or without modification, are permitted
#provided that the following conditions are met:
#
#* Redistributions of source code must retain the above copyright notice, this list of conditions
#  and the following disclaimer.  
#* Redistributions in binary form must reproduce the above copyright notice, this list of 
#  conditions and the following disclaimer in the documentation and/or other materials provided 
# with the distribution.  
#* Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or 
#  promote products derived from this software without specific prior written permission.
#
#THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS
#OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
#AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
#CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
#DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
#DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
#IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
#OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#--------------------------------------------------------------------------------------------------
#
#Author: M Gopinathan
#Email id: gopinathan18@gmail.com
#Details:
#
#--------------------------------------------------------------------------------------------------

##############################################################################
## Preset global variables and attributes
##############################################################################
set RTL $env(SHAKTI_HOME)/verilog
set DESIGN <TOP_MODULE_NAME>
set SYN_EFF high
###set MAP_EFF medium
set MAP_EFF high
exec ls $RTL/ > rtl_files.txt
set fp [open "../work/rtl_files.txt" r]
set rtl_list [read $fp]
set DATE [clock format [clock seconds] -format "%b%d-%T"] 
set _OUTPUTS_PATH outputs_${DATE}
set _REPORTS_PATH reports_${DATE}
set _LOG_PATH logs_${DATE}

##set_attribute hdl_vhdl_read_version 1993 /

##set ET_WORKDIR <ET work directory>
set_attribute lib_search_path {. <Paths to standard cell libraries separated by space>}  
set_attribute script_search_path {../constraints}
set_attribute hdl_search_path $RTL
set_attribute library {<Specify all library.lib separated by space>}
