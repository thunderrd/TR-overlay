for i in $(cat /var/lib/portage/world)
do
	emerge -cp ${i} | grep "All selected packages" &>/dev/null || echo ${i} can be removed
done
