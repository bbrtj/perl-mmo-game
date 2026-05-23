package i18n::Core::i_default;

use v5.42;
use parent 'i18n::Core';

# NOTE: i-default is defined as first fallback language in Locale::Maketext -
# we use it as auto-only

our %Lexicon = (
	_AUTO => true,
);

