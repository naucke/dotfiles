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
    echo "? K"
  elif [ $kib -gt 1024 ]; then
    local mib_int=$(( kib >> 10 ))
    local mib_dec=$(( kib % 1024 * 976 / 10000 ))
    if [ "$mib_dec" -lt 10 ]; then
      mib_dec="0${mib_dec}"
    fi
    echo "${mib_int}.${mib_dec}M"
  else
    echo "${kib}K"
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
    rate=" $(readable $(( (tx - last_tx) / interval ))) |  $(readable $(( (rx - last_rx) / interval )))"
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
  local metrics=$(curl localhost:9101/metrics/ 2> /dev/null)
  if [ ! $metrics ]
  then
    load=""
    return
  fi

  local util=$(printf "%02d%%" $(get_metric "$metrics" utilization_gpu))
  local temp=$(printf "%02d°C" $(get_metric "$metrics" temperature_gpu))

  local used=$(get_metric "$metrics" memory_used)
  local total=$(get_metric "$metrics" memory_total)
  local usage=$(printf "%.1f%%" $(bc -l <<< "100 * $used / $total"))

  load="  $util |  $temp |  $usage |"
}

i3status | (while :
do
  read line
  update_rate
  update_load
  echo "$rate |$load $line" || exit 1
done)
