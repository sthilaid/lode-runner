SPRITE_FILES = $(wildcard sprites/*.ppm) $(wildcard sprites/*.bmp)
FONT_FILES = $(wildcard fonts/*.ppm) $(wildcard fonts/*.scm)
SOUND_FILES = $(wildcard sounds/*.wav)

SCM_LIB_FILES = scm-lib.scm scm-lib-macro.scm
GL_FILES = opengl.scm glu.scm
FONT_FILES = ppm-reader.scm texture.scm sprite.scm font.scm
IMAGE_FILE = user-interface-images.scm
ENGINE_FILES = game-engine.scm level-loader.scm
UI_FILES = sdl-interface.scm user-interface.scm

DEVEL_FILES = $(SCM_LIB_FILES) $(GL_FILES) $(FONT_FILES) $(IMAGE_FILE)
GAME_FILES =  $(SCM_LIB_FILES) $(GL_FILES) $(FONT_FILES) $(IMAGE_FILE) $(ENGINE_FILES) $(UI_FILES)

## compilers
GSC=$(PATH_TO_GAMBIT)/bin/gsc -:=$(PATH_TO_GAMBIT) -debug
CC=gcc

### PATHS

## Gambit-c
PATH_TO_GAMBIT=/opt/gambit-c
GAMBIT_LIB=$(PATH_TO_GAMBIT)/lib
GAMBIT_INCLUDE=$(PATH_TO_GAMBIT)/include

## Some scheme libraries paths
OOSYSYEM_PATH=$(HOME)/projet/maitrise/class
SCMLIB_PATH=$(HOME)/projet/maitrise/scm-lib
SDLINTERFACE_PATH=$(HOME)/projet/maitrise/sdl-interface
OPENGL-FFI_PATH=$(HOME)/projet/scheme/opengl-ffi
GL-FONT_PATH=$(HOME)/projet/maitrise/gl-fonts
SDL-INTERFACE_PATH = $(HOME)/projet/maitrise/sdl-interface

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

all: welcome run-game

devel: $(DEVEL_FILES:.scm=.o1) $(ENGINE_FILES) $(UI_FILES:.scm=.o1)
	gsi -:dar $^ -e '(main)'

run-game: $(GAME_FILES:.scm=.o1)
	@echo "*** Compilation Finished ***"
	@echo
	@echo "Launching game...."
	@echo
	gsi -:dar $(GAME_FILES:.scm=.o1) -e '(main)'

### "included" macro dependant scheme source files

user-interface-images.o1: texture-macro.scm font-macro.scm scm-lib-macro.scm
user-interface.o1: scm-lib-macro.scm opengl-header.scm
game-engine.o1 game-engine.scm: scm-lib-macro.scm class.scm class_.scm
level-loader.o1 level-loader.scm: class_.scm
opengl.o1: opengl-header.scm
glu.o1: glu-header.scm


### External Scheme library dependencies

## Object system
IMPORTED-FILES = class.scm class_.scm
class.scm: $(OOSYSYEM_PATH)/class.scm
	cp $(OOSYSYEM_PATH)/class.scm .
class_.scm: $(OOSYSYEM_PATH)/class_.scm
	cp $(OOSYSYEM_PATH)/class_.scm .

## Scheme basic block library
IMPORTED-FILES += scm-lib.scm scm-lib-macro.scm
scm-lib.scm: $(SCMLIB_PATH)/scm-lib.scm
	cp $(SCMLIB_PATH)/scm-lib.scm .
scm-lib-macro.scm: $(SCMLIB_PATH)/scm-lib-macro.scm
	cp $(SCMLIB_PATH)/scm-lib-macro.scm .

# ## Thread simulation library
# IMPORTED-FILES += match.scm rbtree.scm \
#                   thread-simulation thread-simulation-macro.scm
# match.scm: $(THRDSIM_PATH)/match.scm
# 	cp $(THRDSIM_PATH)/match.scm .
# rbtree.scm: $(THRDSIM_PATH)/rbtree.scm
# 	cp $(THRDSIM_PATH)/rbtree.scm .
# thread-simulation.scm: $(THRDSIM_PATH)/thread-simulation.scm
# 	cp $(THRDSIM_PATH)/thread-simulation.scm .
# thread-simulation-macro.scm: $(THRDSIM_PATH)/thread-simulation-macro.scm
# 	cp $(THRDSIM_PATH)/thread-simulation-macro.scm .

## Opengl ffi
IMPORTED-FILES += opengl.scm opengl-header.scm glu.scm glu-header.scm
opengl.scm: $(OPENGL-FFI_PATH)/opengl.scm
	cp $(OPENGL-FFI_PATH)/opengl.scm .
opengl-header.scm: $(OPENGL-FFI_PATH)/opengl-header.scm
	cp $(OPENGL-FFI_PATH)/opengl-header.scm .
glu.scm: $(OPENGL-FFI_PATH)/glu.scm
	cp $(OPENGL-FFI_PATH)/glu.scm .
glu-header.scm: $(OPENGL-FFI_PATH)/glu-header.scm
	cp $(OPENGL-FFI_PATH)/glu-header.scm .

## Font system
IMPORTED-FILES += font.scm font-macro.scm sprite.scm sprite-macro.scm \
									texture.scm texture-macro.scm ppm-reader.scm
font.scm: $(GL-FONT_PATH)/font.scm
	cp $(GL-FONT_PATH)/font.scm .
font-macro.scm: $(GL-FONT_PATH)/font-macro.scm
	cp $(GL-FONT_PATH)/font-macro.scm .
sprite.scm: $(GL-FONT_PATH)/sprite.scm
	cp $(GL-FONT_PATH)/sprite.scm .
sprite-macro.scm: $(GL-FONT_PATH)/sprite-macro.scm
	cp $(GL-FONT_PATH)/sprite-macro.scm .
texture.scm: $(GL-FONT_PATH)/texture.scm
	cp $(GL-FONT_PATH)/texture.scm .
texture-macro.scm: $(GL-FONT_PATH)/texture-macro.scm
	cp $(GL-FONT_PATH)/texture-macro.scm .
ppm-reader.scm: $(GL-FONT_PATH)/ppm-reader.scm
	cp $(GL-FONT_PATH)/ppm-reader.scm .

## interface to sdl
IMPORTED-FILES += sdl-interface.scm
sdl-interface.scm: $(SDL-INTERFACE_PATH)/sdl-interface.scm
	cp $(SDL-INTERFACE_PATH)/sdl-interface.scm .


## Basic rule for compiling scheme files

.scm.o1:
	$(GSC) -cc-options "$(INCLUDE_OPTIONS)" -ld-options "$(LD_OPTIONS)" -o $*.o1 $*.scm


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

ALL_SCM = $(wildcard *.scm)
clean:
	rm -f $(ALL_SCM:.scm=.c) *_.c *.o* *.exe *.tar.gz *.tgz *.~*~ *.zip
  # external libs
	rm -f $(IMPORTED-FILES)
#$(MAKE) clean -C doc

# tarball: makefile README $(wildcard *.scm) $(SPRITE_FILES) $(FONT_FILES) $(DOC_FILES) $(SOUND_FILES)
# 	tar cvzf space-invaders-src-v$(VERSION).tgz $(foreach file, $^, ../space-invaders/$(file))

