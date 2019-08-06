project('LAPACK95', 'fortran',
  default_options : ['default_library=static', 'libdir=lapack95/lib/'],
  version : '1.0.0',
  meson_version: '>=0.50')

fc = meson.get_compiler('fortran')
if fc.get_id() == 'gcc'
  add_global_arguments( '-fimplicit-none','-Wno-tabs', '-Wno-conversion', '-Wno-unused-label', language : 'fortran')
endif

realkind = get_option('realkind')
subdir('src')

lapack = dependency('lapack')
lapack95 = library('lapack95',
  sources: [src, aux],
  dependencies: lapack,
  install : true)

# --- generate pkg-config
pkg = import('pkgconfig')
pkg.generate(lapack95, url: 'https://github.com/scivision/LAPACK95', subdirs: 'lapack95')

# --- tests

gbsv = executable('gbsv', 'EXAMPLES1' / 'la_gbsv_example.f90',
include_directories : lapack95.private_dir_include(),
link_with : lapack95)

test('GBSV', gbsv,
  args : meson.source_root() /'EXAMPLES1')
