use testheader;

# i18n should be loaded by the header already

# undefine language for tests
local $i18n::CURRENT_LANG;

isa_ok _t('err.out_of_range'), 'i18n';
ok lives { _t('err.out_of_range') }, 'no lang translation lives before stringification ok';
like dies { _t('err.out_of_range') . '' }, qr/no lang/, 'no lang translation dies ok';

is _tt('out of range [_1]', 'test'), 'out of range test', 'no lang translation with _tt ok';
local $i18n::CURRENT_LANG = 'pl';
is _t('err.out_of_range'), 'cel znajduje się poza zasięgiem', 'translation with _t ok';

done_testing;
