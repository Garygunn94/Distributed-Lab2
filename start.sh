#!/bin/bash
function int-ip { /sbin/ifconfig $1 | grep "inet addr" | awk -F: '{print $2}' | awk '{print $1}'; }
#address=$(int-ip eth0)
address=localhost
port=${1-8080}

./Skeleton-Server $address $port
