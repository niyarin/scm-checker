" Author: Niyarin <niyarium@gmail.com>
" Description: Scm-checker support

function! ale_linters#scheme#scm_checker#Handle(buffer, lines) abort
    let l:pattern = '\v^[a-zA-Z]?:?[^:]+:(\d+)?:(\d+)?:?((W):(.+))$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        let l:type = 'E'

        if l:match[4] is? 'W'
            let l:type = 'W'
        endif

        call add(l:output, {
        \   'lnum': l:match[1] + 0,
        \   'col': l:match[2] + 0,
        \   'text': l:match[5],
        \   'type': l:type,
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('scheme', {
\   'name': 'scm-checker',
\   'output_stream': 'stdout',
\   'executable': 'scm-checker',
\   'command': 'scm-checker %s',
\   'callback': 'ale_linters#scheme#scm_checker#Handle',
\})
