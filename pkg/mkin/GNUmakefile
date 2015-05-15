PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename $(PWD))
TGZ     := ../$(PKGSRC)_$(PKGVERS).tar.gz
TGZVNR  := ../$(PKGSRC)_$(PKGVERS)-vignettes-not-rebuilt.tar.gz

# Specify the directory holding R binaries. To use an alternate R build (say a
# pre-prelease version) use `make RBIN=/path/to/other/R/` or `export RBIN=...`
# If no alternate bin folder is specified, the default is to use the folder
# containing the first instance of R on the PATH.
RBIN ?= $(shell dirname "`which R`")

# Specify package and static documentation directories for subversion on r-forge
RFSVN ?= $(HOME)/svn/kinfit.r-forge
RFDIR ?= $(RFSVN)/pkg/mkin
SDDIR ?= $(RFSVN)/www/mkin_static

pkgfiles = NEWS \
	   data/* \
	   DESCRIPTION \
	   inst/staticdocs/README \
	   man/* \
	   NAMESPACE \
	   R/* \
	   README.md \
	   tests/* \
	   tests/testthat* \
	   TODO \
	   vignettes/*

all: NEWS check clean

# convert markdown to R's NEWS format (from knitr package)
NEWS: NEWS.md
	sed -e 's/^-/ -/' -e 's/^## *//' -e 's/^#/\t\t/' <NEWS.md | fmt -80 >NEWS

$(TGZ): $(pkgfiles)
	cd ..;\
		"$(RBIN)/R" CMD build $(PKGSRC)

$(TGZVNR): $(pkgfiles)
	cd ..;\
		"$(RBIN)/R" CMD build $(PKGSRC) --no-build-vignettes;\
		cd $(PKGSRC);\
	mv $(TGZ) $(TGZVNR)
                
build: $(TGZ)

build-no-vignettes: $(TGZVNR)

install: build
	"$(RBIN)/R" CMD INSTALL $(TGZ)

install-no-vignettes: build-no-vignettes
	"$(RBIN)/R" CMD INSTALL $(TGZVNR)

check: build
	# Vignettes have been rebuilt by the build target
	"$(RBIN)/R" CMD check --as-cran --no-tests --no-build-vignettes $(TGZ)

check-no-vignettes: build-no-vignettes
	mv $(TGZVNR) $(TGZ)
	"$(RBIN)/R" CMD check --as-cran --no-tests $(TGZ)
	mv $(TGZ) $(TGZVNR)

clean: 
	$(RM) -r $(PKGNAME).Rcheck/
	$(RM) vignettes/*.R

test: install-no-vignettes
	cd tests;\
		"$(RBIN)/Rscript" testthat.R

vignettes/mkin.pdf: vignettes/mkin.Rnw
	"$(RBIN)/Rscript" -e "tools::buildVignette(file = 'vignettes/mkin.Rnw', dir = 'vignettes')"

vignettes/FOCUS_L.html: vignettes/FOCUS_L.Rmd
	"$(RBIN)/Rscript" -e "tools::buildVignette(file = 'vignettes/FOCUS_L.Rmd', dir = 'vignettes')"

vignettes/FOCUS_Z.pdf: vignettes/FOCUS_Z.Rnw
	"$(RBIN)/Rscript" -e "tools::buildVignette(file = 'vignettes/FOCUS_Z.Rnw', dir = 'vignettes')"

vignettes: vignettes/mkin.pdf vignettes/FOCUS_L.html vignettes/FOCUS_Z.pdf

sd:
	"$(RBIN)/Rscript" -e "library(staticdocs); build_site()"

move-sd:
	rm -rf $(SDDIR)/*;\
	cp -r inst/web/* $(SDDIR); cd $(SDDIR) && svn add --force .

r-forge: sd move-sd
	git archive master > $(HOME)/mkin.tar;\
	cd $(RFDIR) && rm -r `ls` && tar -xf $(HOME)/mkin.tar;\
	svn add --force .; cd $(RFSVN) && svn commit -m 'sync with git'

winbuilder: build
	date
	@echo "Uploading to R-release on win-builder"
	curl -T $(TGZ) ftp://anonymous@win-builder.r-project.org/R-release/
	@echo "Uploading to R-devel on win-builder"
	curl -T $(TGZ) ftp://anonymous@win-builder.r-project.org/R-devel/

submit:
	@echo "\nHow about make test, make check, make winbuilder"
	@echo "\nIs the DESCRIPTION file up to date?"
	@echo "\nIs the NEWS.md file up to date?"
	@echo "\nAre you sure you want to release to CRAN?"
	@echo "\nThen make r-forge, commit leftover changes if any, tag the release and use the form at http://cran.r-project.org/submit.html"
