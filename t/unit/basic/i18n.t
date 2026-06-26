# HARNESS-DURATION-SHORT
use testheader;

# i18n should be loaded by the header already

# undefine language for tests
local $i18n::CURRENT_LANG = undef;

isa_ok _t(Err::OUT_OF_RANGE), 'i18n::Translation';
ok lives { _t(Err::OUT_OF_RANGE) }, 'no lang translation lives before stringification ok';
like dies { _t(Err::OUT_OF_RANGE) . '' }, qr/no lang/, 'no lang translation dies ok';

is _tt('out of range [_1]', 'test'), 'out of range test', 'no lang translation with _tt ok';
local $i18n::CURRENT_LANG = 'pl';
is _t(Err::OUT_OF_RANGE), 'cel znajduje się poza zasięgiem', 'translation with _t ok';

done_testing;

