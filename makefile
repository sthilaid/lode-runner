## utility
add-presufix = $(foreach f, $(3), $(1)$(f)$(2))
define define-dependency 
$(addprefix $$(SRC_PATH)/,$(2)) : setup-$(1)
setup-$(1):
	 mkdir -p $$(INCLUDE_PATH)
	 mkdir -p $$(LIB_PATH)
	 mkdir -p $$(EXTERNAL_LIBS)
 ifeq "$$(wildcard $$(EXTERNAL_LIBS)/$(1))" ""
	 cd $$(EXTERNAL_LIBS) && git clone $$($(1)-PATH)
 endif
	 cd $$(EXTERNAL_LIBS)/$(1) && git pull
	 $$(MAKE) -C $$(EXTERNAL_LIBS)/$(1)
	 rsync -c $$(EXTERNAL_LIBS)/$(1)/include/* $$(SRC_PATH)/
	 rsync -c $$(EXTERNAL_LIBS)/$(1)/include/* $$(INCLUDE_PATH)/
	 rsync -c $$(EXTERNAL_LIBS)/$(1)/src/* $$(SRC_PATH)/
	 rsync -c $$(EXTERNAL_LIBS)/$(1)/lib/* $$(LIB_PATH)/
endef

generate-dependency = $(eval $(call define-dependency,$(1),$(2)))

## project paths
PREFIX=.
SRC_PATH=src
INCLUDE_PATH=$(PREFIX)/include
LIB_PATH=$(PREFIX)/lib
EXTERNAL_LIBS=$(PREFIX)/external-libs

## Projet files
SPRITE_FILES = $(wildcard sprites/*.ppm) $(wildcard sprites/*.bmp)
FONT_FILES = $(wildcard fonts/*.ppm) $(wildcard fonts/*.scm)
SOUND_FILES = $(wildcard sounds/*.wav)
FONT_IMAGES   = wall ladder handbar gold player title_bar robot logo bb_fonts

INCLUDE_FILES = declarations.scm \
                scm-lib_.scm class.scm class_.scm opengl_.scm glu_.scm \
                texture_.scm sprite_.scm font_.scm \
                class_.scm thread-simulation_.scm match.scm
LIB_FILES = scm-lib.o1 opengl.o1 glu.o1 ppm-reader.o1 texture.o1 sprite.o1 \
            font.o1 sdl-interface.o1 thread-simulation.o1 rbtree.o1 format.o1 \
            level-loader.o1 game-engine.o1 user-interface.o1 statprof.o1
GAME_FILES = $(LIB_FILES) $(call add-presufix,font-,.o1,$(FONT_IMAGES))


## compilers
GSC=$(PATH_TO_GAMBIT)/bin/gsc -:=$(PATH_TO_GAMBIT) -debug
CC=gcc

## Gambit-c location
PATH_TO_GAMBIT=/opt/gambit-c
GAMBIT_LIB=$(PATH_TO_GAMBIT)/lib
GAMBIT_INCLUDE=$(PATH_TO_GAMBIT)/include

## Some scheme libraries git repos
# class-PATH=git://github.com/sthilaid/class.git
# thread-simulation-PATH=git://github.com/sthilaid/thread-simulation.git
# scm-lib-PATH=git://github.com/sthilaid/scm-lib.git
# open-gl-ffi-PATH=git://github.com/sthilaid/open-gl-ffi.git
# gl-fonts-PATH=git://github.com/sthilaid/gl-fonts.git
# sdl-interface-PATH=git://github.com/sthilaid/sdl-interface.git

class-PATH=../../class
thread-simulation-PATH=/home/dave/projet/maitrise/thread-simulation
scm-lib-PATH=/home/dave/projet/maitrise/scm-lib
open-gl-ffi-PATH=/home/dave/projet/scheme/open-gl-ffi
gl-fonts-PATH=/home/dave/projet/maitrise/gl-fonts
sdl-interface-PATH=/home/dave/projet/maitrise/sdl-interface

export class-PATH 
export thread-simulation-PATH
export scm-lib-PATH
export open-gl-ffi-PATH
export gl-fonts-PATH
export sdl-interface-PATH

## Comilation flags
LD_OPTIONS_LIN = -lutil -lSDL -lSDL_mixer -lglut
LD_OPTIONS_COMMON =-L$(GAMBIT_LIB) -L$(GL_LIB) -L$(SDL_LIB) -L$(SDL_mixer_LIB) -lgambc 

PATH_TO_GL=/usr
GL_INCLUDE=$(PATH_TO_GL)/include/GL
GL_LIB=$(PATH_TO_GL)/lib

PATH_TO_SDL=/usr
SDL_INCLUDE=$(PATH_TO_SDL)/include/SDL
SDL_LIB=$(PATH_TO_SDL)/lib

PATH_TO_SDL_mixer=/usr
SDL_mixer_INCLUDE=$(PATH_TO_SDL_mixer)/include/SDL
SDL_mixer_LIB=$(PATH_TO_SDL_mixer)/lib

ALL_SDL_INCLUDE=-I$(SDL_INCLUDE) -I$(SDL_mixer_INCLUDE)

INCLUDE_OPTIONS=-I$(GAMBIT_INCLUDE) -I$(GL_INCLUDE) $(ALL_SDL_INCLUDE)
LD_OPTIONS = $(LD_OPTIONS_COMMON) $(LD_OPTIONS_LIN)


.SUFFIXES:
.SUFFIXES: .c .scm .o .o1 .m
.PHONY: all clean shared-objects tarball welcome

all: welcome prefix include lib

prefix:
ifneq "$(PREFIX)" "."
	mkdir -p $(PREFIX)
endif

include: $(foreach f,$(INCLUDE_FILES),$(INCLUDE_PATH)/$(f))
$(INCLUDE_PATH)/%.scm: $(SRC_PATH)/%.scm
	mkdir -p $(INCLUDE_PATH)
	cp $< $@

lib: $(foreach f,$(GAME_FILES),$(LIB_PATH)/$(f))
$(LIB_PATH)/%.o1: $(SRC_PATH)/%.scm
	mkdir -p $(LIB_PATH)
	$(GSC) -cc-options "$(INCLUDE_OPTIONS)" -ld-options "$(LD_OPTIONS)" -o $@ $<

stringify = $(foreach f,$(1),"$(f)")
devel: $(SRC_PATH)/game-loader.scm \
       $(call add-presufix,$(LIB_PATH)/font-,.o1,$(FONT_IMAGES))
	gsi -:dar $< -e '(load-game $(call stringify,$(SRC_PATH)) $(call stringify,$(LIB_PATH)) (list $(call stringify,$(GAME_FILES))))'

run-game: $(addprefix $(LIB_PATH)/,$(GAME_FILES))
	@echo "*** Compilation Finished ***"
	@echo
	@echo "Launching game...."
	@echo
	gsi -:dar $^ -e '(main)'

$(LIB_PATH)/font-%.o1: generated/font-%.scm
	mkdir -p $(LIB_PATH)
	$(GSC) -cc-options "$(INCLUDE_OPTIONS)" -ld-options "$(LD_OPTIONS)" -o $@ $<

generated/font-%.scm: fonts/%.ppm $(SRC_PATH)/user-interface-images.scm
	@echo Generating font scm file $@
	mkdir -p generated
	gsi $(SRC_PATH)/user-interface-images.scm -e "(generate-font-file \"$(<)\")"

static: $(addprefix $(SRC_PATH)/,$(LIB_FILES:.o1=.scm)) \
        $(call add-presufix,generated/font-,.scm,$(FONT_IMAGES))
	$(GSC) -exe -o $(PREFIX)/lode-runner -cc-options "$(INCLUDE_OPTIONS)" -ld-options "$(LD_OPTIONS)" $^

### "included" macro dependant scheme source files

%.o1: $(INCLUDE_PATH)/declarations.scm
user-interface-images.o1: $(addprefix $(INCLUDE_PATH),texture_.scm \
                                                      font_.scm scm-lib_.scm)
user-interface.o1: $(addprefix $(INCLUDE_PATH),scm-lib_.scm opengl_.scm)
game-engine.o1 game-engine.scm: $(addprefix $(INCLUDE_PATH),scm-lib_.scm\
                                   class.scm class_.scm)
level-loader.o1 level-loader.scm: $(INCLUDE_PATH)/class_.scm
scm-lib.o1: $(INCLUDE_PATH)/scm-lib_.scm
opengl.o1: $(INCLUDE_PATH)/opengl_.scm
glu.o1: $(addprefix $(INCLUDE_PATH),opengl_.scm glu_.scm)
$(call add-presufix,generated/font-,.scm,$(FONT_IMAGES)) : $(addprefix $(INCLUDE_PATH)/,texture_.scm font_.scm declarations.scm)

## Welcome banner
 welcome:
	@echo "*** Currently using following paths ***"
	@echo
	@echo PATH_TO_GAMBIT=$(PATH_TO_GAMBIT)
	@echo PATH_TO_GL=$(PATH_TO_GL)
ifeq ($(UI), sdl)
	@echo PATH_TO_SDL=$(PATH_TO_SDL)
	@echo PATH_TO_SDL_mixer=$(PATH_TO_SDL_mixer)
endif
	@echo
	@echo "*** Beginning Compilation ***"

### External Scheme library dependencies
$(eval $(call define-dependency,scm-lib,scm-lib.scm scm-lib_.scm))
$(eval $(call define-dependency,open-gl-ffi,opengl.scm opengl_.scm \
                                            glu.scm glu_.scm))
$(eval $(call generate-dependency,sdl-interface,sdl-interface.scm))
$(eval $(call generate-dependency,gl-fonts,ppm-reader.scm \
                                          texture.scm texture_.scm \
	                                        sprite.scm sprite_.scm \
	                                        font.scm font_.scm))
$(eval $(call generate-dependency,class,class.scm class_.scm))
$(eval $(call generate-dependency,thread-simulation, thread-simulation.scm \
                                  thread-simulation_.scm rbtree.scm match.scm))


clean:
	rm -rf generated $(INCLUDE_PATH) $(LIB_PATH) $(EXTERNAL_LIBS) $(SRC_PATH)/*.[oc] $(PREFIX)/lode-runner