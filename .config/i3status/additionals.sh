#!/bin/sh
# derived from https://github.com/i3/i3status/blob/master/contrib/net-speed.sh

ifaces="enp3s0"

last_time=0
last_rx=0
last_tx=0
rate=""

load=""

readable() {
  local bytes=$1
  local kib=$(( bytes >> 10 ))
  if [ $kib -lt 0 ]; then
    readable_rate="? K"
  elif [ $kib -gt 1024 ]; then
    local mib_int=$(( kib >> 10 ))
    local mib_dec=$(( kib % 1024 * 976 / 10000 ))
    if [ "$mib_dec" -lt 10 ]; then
      mib_dec="0${mib_dec}"
    fi
    readable_rate="${mib_int}.${mib_dec}M"
  else
    readable_rate="${kib}K"
  fi
}

update_rate() {
  local time=$(date +%s)
  local rx=0 tx=0 tmp_rx tmp_tx

  for iface in $ifaces; do
    read tmp_rx < "/sys/class/net/${iface}/statistics/rx_bytes"
    read tmp_tx < "/sys/class/net/${iface}/statistics/tx_bytes"
    rx=$(( rx + tmp_rx ))
    tx=$(( tx + tmp_tx ))
  done

  local interval=$(( $time - $last_time ))
  if [ $interval -gt 0 ]; then
    readable $(( (tx - last_tx) / interval ))
    local up=$readable_rate
    readable $(( (rx - last_rx) / interval ))
    rate=" $up |  $readable_rate"
  else
    rate=""
  fi

  last_time=$time
  last_rx=$rx
  last_tx=$tx
}

get_metric() {
  grep $2 <<< $1 | awk '{print $NF}'
}

update_load() {
  local metrics=$(curl --max-time .5 localhost:9101/metrics/ 2> /dev/null)
  if [ ! $metrics ]
  then
    load=""
    return
  fi

  local util=$(printf "%02d%%" $(get_metric "$metrics" utilization_gpu))
  local temp=$(printf "%02d°C" $(get_metric "$metrics" temperature_gpu))
  local usage=$(printf "%02d%%" $(get_metric "$metrics" utilization_memory))

  load="  $util |  $temp |  $usage |"
}

i3status | (while :
do
  read line
  update_rate
  update_load
  echo "$rate |$load $line" || exit 1
done)
