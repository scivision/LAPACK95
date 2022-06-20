include(FetchContent)

file(READ ${CMAKE_CURRENT_LIST_DIR}/libraries.json json)

string(JSON lapack95_url GET ${json} lapack95 url)

set(FETCHCONTENT_QUIET no)

FetchContent_Declare(LAPACK95
URL ${lapack95_url}
INACTIVITY_TIMEOUT 60
)

FetchContent_Populate(LAPACK95)
