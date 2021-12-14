#!gmake -f

udh: udh-master

# As a backup for when master changes
udh-%:
	# -L to follow symlinks
	rsync -avzL $(@:udh-%=%).debian.org:/var/lib/misc/ssh_known_hosts private_dot_ssh/known_hosts.d/debian

.PHONY: udh
