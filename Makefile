
include Makefile.stub

.PHONY: all

BLD=prod
COMPS=lib
# lib/tools/toolsdev

# ==================================================== build

all:
	$(GPRBUILD) -p -j0 -XBLD=$(BLD) -XOPSYS=$(OPSYS) \
		$(GPRBUILD_FLAGS) -XASIS_COMPONENTS=$(COMPS) build_asis.gpr

tools: setup
	$(GPRBUILD) -p -j0 -XBLD=$(BLD) -XOPSYS=$(OPSYS) \
		$(GPRBUILD_FLAGS) -XASIS_COMPONENTS=tools build_asis.gpr

setup:
	$(GPRBUILD) -p  -XBLD=$(BLD) tools/tool_utils/generate_factory.gpr
	rm -rf tools/tool_utils/ada_trees-factory.ads tools/tool_utils/ada_trees-factory.adb
	rm -rf tools/gnat2xml/ada_trees-factory.ads tools/gnat2xml/ada_trees-factory.adb
	cd tools/tool_utils ; ./ada_trees-generate_factory$(exe_ext)


# ==================================================== install

install-clean:
ifneq (,$(wildcard $(prefix)/lib/gnat/manifests/asislib))
	-$(GPRINSTALL) --uninstall --prefix=$(prefix) \
		--project-subdir=lib/gnat asislib
endif

install: install-clean
	$(GPRINSTALL) -p -f --prefix=$(prefix) --sources-subdir=include/asis \
		--lib-subdir=lib/asis --project-subdir=lib/gnat -XBLD=$(BLD) \
		-XOPSYS=$(OPSYS) -XASIS_COMPONENTS=lib build_asis.gpr

install-tools-clean:
ifneq (,$(wildcard $(prefix)/lib/gnat/manifests/asistools))
	-$(GPRINSTALL) --uninstall --prefix=$(prefix) \
		--project-subdir=lib/gnat asistools
endif

install-tools: install-tools-clean
	$(GPRINSTALL) -p -f --prefix=$(prefix) --sources-subdir=include/asis \
		--lib-subdir=lib/asis --project-subdir=lib/gnat -XBLD=$(BLD) \
		-XOPSYS=$(OPSYS) -XASIS_COMPONENTS=tools build_asis.gpr

# ==================================================== test

gnattest_hash_testing:
	$(GPRBUILD) "-Pinternal/tools/gnattest/hash_testing/$@" "-XBLD=$(BLD)" "-XOPSYS=$(OPSYS)"

# ==================================================== clean

clean:
	-$(GPRCLEAN) -XASIS_COMPONENTS=lib build_asis.gpr
	-$(GPRCLEAN) -XASIS_COMPONENTS=toolsdev build_asis.gpr
	-$(GPRCLEAN) tools/tool_utils/generate_factory.gpr
	rm -f tools/tool_utils/ada_trees-factory.ads tools/tool_utils/ada_trees-factory.adb
