#!/bin/bash
## created on 2015-04-07

#### Setup and enable a usb irda device
## used to readout Polar watch

modprobe uhci_hcd
modprobe pl2303
modprobe irda
#echo 115200 > /proc/sys/net/irda/max_baud_rate
echo 57600 > /proc/sys/net/irda/max_baud_rate
modprobe ma600-sir
modprobe ircomm-tty
irattach /dev/ttyUSB0 -d ma600 -s
rm -rf /dev/modem
ln -s /dev/ircomm0 /dev/modem


exit 0
