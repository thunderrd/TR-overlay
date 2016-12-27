# Copyright 1999-2016 Gentoo Foundation
# Distributed under the terms of the GNU General Public License v2
# $Id$

EAPI="6"
inherit autotools eutils git-r3

DESCRIPTION="CompizConfig plugin required for compizconfig-settings-manager"
HOMEPAGE="https://github.com/compiz-reloaded"
EGIT_REPO_URI="git://github.com/compiz-reloaded/libcompizconfig.git"

LICENSE="GPL-2+"
SLOT="0"

RDEPEND="dev-libs/libxml2
	dev-libs/protobuf
	x11-libs/libX11
	>=x11-wm/compiz-${PV}
"
DEPEND="${RDEPEND}
	>=dev-util/intltool-0.41
	virtual/pkgconfig
	x11-proto/xproto
"

RESTRICT="test"

src_prepare() {
	default
	eautoreconf
}

src_configure() {
	econf \
		--enable-fast-install \
		--disable-static
}

src_install() {
	default
	prune_libtool_files --all
}
