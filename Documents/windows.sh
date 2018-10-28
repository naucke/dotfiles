#!/bin/bash

# USB Devices
export USB_BUS=`lsusb | grep "Logitech, Inc\. G402 Gaming Mouse" | sed -r "s/^Bus 0{0,2}([1-9][0-9]{0,2}) Device .*/\1/"`
export X360=`lsusb | grep "Microsoft Corp\. Xbox 360 Wireless Adapter" | sed -r "s/^Bus [0-9]{3} Device 0{0,2}([1-9][0-9]{0,2}): ID .*/\1/"`
export MOUSE=`lsusb | grep "Logitech, Inc\. G402 Gaming Mouse" | sed -r "s/^Bus [0-9]{3} Device 0{0,2}([1-9][0-9]{0,2}): ID .*/\1/"`
export DAC=`lsusb | grep "Texas Instruments PCM2902 Audio Codec" | sed -r "s/^Bus [0-9]{3} Device 0{0,2}([1-9][0-9]{0,2}): ID .*/\1/"`

# Looking Glass
touch /dev/shm/looking-glass
chown jakob:kvm /dev/shm/looking-glass
chmod 660 /dev/shm/looking-glass

# Avalanche Studios edge case
echo 1 > /sys/module/kvm/parameters/ignore_msrs

# Unmount HDD
umount /mnt/HDD

# Bind GPU
vfio-bind 0000:01:00.0 0000:01:00.1

# BIOS
cp /usr/share/edk2.git/ovmf-x64/OVMF_VARS-pure-efi.fd /tmp/my_vars.fd

# QEMU
qemu-system-x86_64 \
  -enable-kvm \
  -m 12288 \
  -smp cores=4,threads=2 \
  -cpu host,kvm=off \
  -vga none \
  -device vfio-pci,host=01:00.0,multifunction=on \
  -device vfio-pci,host=01:00.1 \
  -device ivshmem-plain,memdev=ivshmem,bus=pci.0 \
  -object memory-backend-file,id=ivshmem,share=on,mem-path=/dev/shm/looking-glass,size=32M \
  -spice port=5900,addr=127.0.0.1,disable-ticketing \
  -usb -device usb-host,hostbus=$USB_BUS,hostaddr=$X360 \
  -usb -device usb-host,hostbus=$USB_BUS,hostaddr=$MOUSE \
  -usb -device usb-host,hostbus=$USB_BUS,hostaddr=$DAC \
  -drive if=pflash,format=raw,readonly,file=/usr/share/edk2.git/ovmf-x64/OVMF_CODE-pure-efi.fd \
  -drive if=pflash,format=raw,file=/tmp/my_vars.fd \
  -device virtio-scsi-pci,id=scsi \
  -drive file=/home/jakob/Documents/VMs/win.img,id=disk,format=qcow2,if=none,cache=writeback -device scsi-hd,drive=disk \
  -drive file=/dev/sdb,id=hdd,format=raw,if=none -device scsi-hd,drive=hdd \

# Mount HDD again
mount -t ntfs-3g /dev/sdb2 /mnt/HDD

# Omitted options: PS/2 keyboard, CD-ROM
#  -object input-linux,id=kbd,evdev=/dev/input/by-path/platform-i8042-serio-0-event-kbd,grab_all=on,repeat=on \
#  -cdrom /dev/cdrom \
