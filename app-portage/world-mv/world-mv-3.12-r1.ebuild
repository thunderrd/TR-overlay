# Copyright 1999-2014 Gentoo Foundation
# Distributed under the terms of the GNU General Public License v2
# $Header: $

EAPI=5
RESTRICT="mirror"
inherit eutils

mPN="${PN%-*}"
DESCRIPTION="Organize your world file and find installed packages or differences to @world"
HOMEPAGE="https://github.com/vaeth/world/"
SRC_URI="https://github.com/vaeth/${mPN}/archive/${PV}.tar.gz -> ${P}.tar.gz"
S="${WORKDIR}/${mPN}-${PV}"

LICENSE="GPL-2"
SLOT="0"
KEYWORDS="~amd64 ~x86"
IUSE=""
S="${WORKDIR}/${mPN}-${PV}"

src_prepare() {
	if use prefix
	then	sed -i \
			-e "s'\${EPREFIX}'\\'${EPREFIX}\\''" \
			-- "${mPN}" || die
	else	sed -i \
			-e '1s"^#!/usr/bin/env sh$"#!'"${EPREFIX}/bin/sh"'"' \
			-- "${mPN}" || die
	fi
	epatch_user
}

src_install() {
	dobin "${mPN}"
	insinto /usr/share/zsh/site-functions
	doins _"${mPN}"
}
