# Copyright 1999-2020 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2

EAPI="6"
ETYPE="sources"
KEYWORDS="~amd64 ~x86"

HOMEPAGE="http://kernel.kolivas.org/"

K_SECURITY_UNSUPPORTED="1"

CK_EXTRAVERSION="ck1"

inherit kernel-2
detect_version
detect_arch

DESCRIPTION="Linux ${K_BRANCH_ID}, with Con Kolivas' MuQSS scheduler and patchset"

K_BRANCH_ID="${KV_MAJOR}.${KV_MINOR}"

#CK_URI="http://ck.kolivas.org/patches/5.0/${K_BRANCH_ID}/${K_BRANCH_ID}-${CK_EXTRAVERSION}/patch-${K_BRANCH_ID}-${CK_EXTRAVERSION}.xz"

SRC_URI="${KERNEL_URI}
	${ARCH_URI}
	"

UNIPATCH_LIST="
	${FILESDIR}/0001-ZEN-Add-sysctl-and-CONFIG-to-disallow-unprivileged-C.patch
	${FILESDIR}/0002-Bluetooth-Fix-LL-PRivacy-BLE-device-fails-to-connect.patch
	${FILESDIR}/0003-Bluetooth-Fix-attempting-to-set-RPA-timeout-when-uns.patch
	${FILESDIR}/0004-HID-quirks-Add-Apple-Magic-Trackpad-2-to-hid_have_sp.patch
	${FILESDIR}/enable_additional_cpu_optimizations_for_gcc_v9.1+_kernel_v5.8+.patch
	${FILESDIR}/0006-init-Kconfig-enable-O3-for-all-arches.patch
        ${FILESDIR}/4567_distro-Gentoo-Kconfig.patch
	${FILESDIR}/5.10-ck.patch
	${FILESDIR}/${CK_EXTRAVERSION}-revert-version.patch
	"

UNIPATCH_STRICTORDER="yes"

src_configure() {
	einfo "Copying Arch config and patches"
	cp ${FILESDIR}/config ${S}/arch-config
	# disable CONFIG_DEBUG_INFO=y at build time otherwise memory usage blows up
	# and can easily overwhelm a system with 32 GB of memory using a tmpfs build
	# partition ... this was introduced by FS#66260, see:
	# https://git.archlinux.org/svntogit/packages.git/commit/trunk?h=packages/linux&id=663b08666b269eeeeaafbafaee07fd03389ac8d7
	sed -i -e 's/CONFIG_DEBUG_INFO=y/# CONFIG_DEBUG_INFO is not set/' \
	-i -e '/CONFIG_DEBUG_INFO_DWARF4=y/d' \
	-i -e '/CONFIG_DEBUG_INFO_BTF=y/d' ${S}/arch-config

	# https://bbs.archlinux.org/viewtopic.php?pid=1824594#p1824594
	sed -i -e 's/# CONFIG_PSI_DEFAULT_DISABLED is not set/CONFIG_PSI_DEFAULT_DISABLED=y/' ${S}/arch-config

	# https://bbs.archlinux.org/viewtopic.php?pid=1863567#p1863567
	sed -i -e '/CONFIG_LATENCYTOP=/ s,y,n,' \
	-i -e '/CONFIG_SCHED_DEBUG=/ s,y,n,' ${S}/arch-config

	# FS#66613
	# https://bugzilla.kernel.org/show_bug.cgi?id=207173#c6
	sed -i -e 's/CONFIG_KVM_WERROR=y/# CONFIG_KVM_WERROR is not set/' ${S}/arch-config
	
	einfo "Default Arch config saved as arch-config in kernel DIR"
	einfo "See https://aur.archlinux.org/packages/linux-ck for latest info"
}
