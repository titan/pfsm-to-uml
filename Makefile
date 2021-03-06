include .config
NAME-LINK=$(subst _,-,$(NAME))

ESCAPED-BUILDDIR = $(shell echo '$(BUILDDIR)' | sed 's%/%\\/%g')
TARGET=$(BUILDDIR)/build/exec/$(NAME-LINK)_app/$(NAME-LINK).so
PKGPREFIX=.
SRCS=$(wildcard $(PKGPREFIX)/*.idr)
DSTSRCS=$(addprefix $(BUILDDIR)/$(PKGPREFIX)/, $(notdir $(SRCS)))
PRJCONF=$(NAME-LINK).ipkg

all: $(TARGET)

install: $(PREFIX)/$(NAME-LINK)

$(PREFIX)/$(NAME-LINK): $(TARGET)
	sudo install $(TARGET) $(PREFIX)/$(NAME-LINK)

$(TARGET): $(DSTSRCS) $(BUILDDIR)/$(PRJCONF) | prebuild
	cd $(BUILDDIR); idris2 --build $(PRJCONF); cd -

$(BUILDDIR)/$(PKGPREFIX)/%.idr: $(PKGPREFIX)/%.idr | prebuild
	cp $< $@

$(BUILDDIR)/$(PRJCONF): $(PRJCONF) | prebuild
	cp $< $@

prebuild:
ifeq "$(wildcard $(BUILDDIR)/$(PKGPREFIX))" ""
	@mkdir -p $(BUILDDIR)/$(PKGPREFIX)
endif

clean:
	@rm -rf $(BUILDDIR)

.PHONY: all clean prebuild
