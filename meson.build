project('LAPACK95', 'fortran',
  default_options : ['default_library=static',
    'libdir=lapack95/lib/', 'includedir=lapack95/include',
    'buildtype=release'],
  version : '1.0.0',
  meson_version: '>=0.50')

fc = meson.get_compiler('fortran')
if fc.get_id() == 'gcc'
  add_project_arguments( '-fimplicit-none','-Wno-tabs', '-Wno-conversion', '-Wno-unused-label', language : 'fortran')
elif fcid == 'intel'
  add_project_arguments('-warn declarations', '-heap-arrays', language : 'fortran')
elif fcid == 'intel-cl'
  add_project_arguments('/warn:declarations', '/heap-arrays', language : 'fortran')
elif fcid == 'pgi'
  add_project_arguments('-C', '-Mdclchk', language : 'fortran')
endif

arith = get_option('arith')
subdir('src')

lapack = dependency('lapack', required: false)
if not lapack.found()
  lapack_proj = subproject('lapack')
  lapack = lapack_proj.get_variable('lapack')
endif

lapack95 = library('lapack95',
  sources: [src, aux],
  dependencies: lapack,
  install : true)

# --- generate pkg-config
pkg = import('pkgconfig')
pkg.generate(lapack95,
  url: 'https://github.com/scivision/LAPACK95',
  subdirs: 'lapack95',
  description: 'Netlib LAPACK95')

# --- tests

gbsv = executable('gbsv', 'EXAMPLES1' / 'la_gbsv_example.f90',
include_directories : lapack95.private_dir_include(),
link_with : lapack95)

test('GBSV', gbsv,
  args : meson.source_root() /'EXAMPLES1')
