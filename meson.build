project('LAPACK95', 'fortran',
  default_options : ['default_library=static', 'libdir=lib/'],
  version : '1.0.0')

fc = meson.get_compiler('fortran')
if fc.get_id() == 'gcc'
  add_global_arguments( '-fimplicit-none','-Wno-tabs', '-Wno-conversion', '-Wno-unused-label', language : 'fortran')
endif

realkind = get_option('realkind')
subdir('src')
if realkind == 's'
  src = [s_src, s_aux]
elif realkind == 'd'
  src = [d_src, d_aux]
elif realkind == 'c'
  src = [c_src, c_aux]
elif realkind == 'z'
  src = [z_src, z_aux]
else
  error('unknown realkind ',realkind)
endif

lapack = dependency('lapack', language: 'fortran')
lapack95 = library('lapack95', src, aux,
  dependencies: lapack,
  install : true)

# --- generate pkg-config
pkg = import('pkgconfig')
pkg.generate(lapack95, url: 'https://github.com/scivision/LAPACK95', subdirs: 'lapack95')

# --- tests

gbsv = executable('gbsv', 'EXAMPLES1' / 'la_gbsv_example.f90',
include_directories : lapack95.private_dir_include(),
link_with : lapack95)

test('GBSV', gbsv, args : 'EXAMPLES1')
