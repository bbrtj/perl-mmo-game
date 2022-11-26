package i18n;

use v5.36;

use Exporter qw(import);
use i18n::Translation;

our @EXPORT = qw(
	_t
	_tt
	_tph
	_lt
);

our $CURRENT_LANG = undef;

# simple translate
# uses a message id that won't get translated directly
sub _t ($message, @args)
{
	my ($actual_message, @more_args) = split /\|/, $message;
	@args = @more_args
		if @more_args > 0;

	return i18n::Translation->new(
		message => $actual_message,
		args => \@args,
	);
}

# text translate
# a text in English that will be returned. Can be used to nest translations
sub _tt ($message, @args)
{
	return i18n::Translation->new(
		message => $message,
		args => \@args,
		id => !!0,
	);
}

# placeholder translate
# will add values to placeholders in the string. Does not actually translate
# used to transfer translation strings and placeholders to client where they are translated
sub _tph (@args)
{
	return join '|', @args;
}

# lore translate
# will translate using database lore data, searching for the id
# the second parameters should be a type - name or description
sub _lt ($message, @args)
{
	my $t = _t $message, @args;
	$t->set_lore(1);

	return $t;
}

1;

