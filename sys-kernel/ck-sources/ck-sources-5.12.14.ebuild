# Copyright 1999-2021 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2

EAPI="6"
ETYPE="sources"
KEYWORDS="~amd64 ~x86"

RESTRICT="test"

HOMEPAGE="http://kernel.kolivas.org"

CK_EXTRAVERSION="ck1"

inherit kernel-2
detect_version
detect_arch

RDEPEND="virtual/linux-sources"

K_BRANCH_ID="5.12"

DESCRIPTION="Linux ${K_BRANCH_ID}, with Con Kolivas' MuQSS scheduler and patchset"

CK_URI="http://ck.kolivas.org/patches/5.0/${K_BRANCH_ID}/${K_BRANCH_ID}-${CK_EXTRAVERSION}/patch-${K_BRANCH_ID}-${CK_EXTRAVERSION}.xz"

SRC_URI="
	${KERNEL_URI}
	${ARCH_URI}
	${CK_URI}
"

UNIPATCH_LIST="
	"${FILESDIR}"/0001-cpu-patches.patch
	"${DISTDIR}"/patch-"${K_BRANCH_ID}"-"${CK_EXTRAVERSION}".xz
"

UNIPATCH_STRICTORDER="yes"

pkg_postinst() {
	kernel-2_pkg_postinst
}

pkg_postrm() {
	kernel-2_pkg_postrm
}
