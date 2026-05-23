package i18n::Translation;

use v5.42;

use My::Moose;
use i18n::Core;
use Types;
use Carp qw(croak);
use DI;

use overload
	q{""} => "translate",
	bool => sub { 1 },
	fallback => 1;

has param 'id' => (
	isa => Types::Bool,
	default => !!1,
);

has param 'lore' => (
	isa => Types::Bool,
	writer => 1,
	default => !!0,
);

has param 'message' => (
	isa => Types::Str,
);

has param 'args' => (
	isa => Types::ArrayRef,
	default => sub { [] },
);

# NOTE: used in overload
sub translate ($self, @)
{
	croak 'could not translate (no lang): ' . $self->message
		if !defined $i18n::CURRENT_LANG && $self->id;

	my $lang = $i18n::CURRENT_LANG // 'en';

	return $self->translate_lore($lang)
		if $self->lore;

	return $self->translate_maketext($lang);

}

sub translate_maketext ($self, $lang)
{
	state %localizers = (auto => i18n::Core->get_handle('i-default'));
	my $localizer = $localizers{$lang} //=
		do {
			my $lh = i18n::Core->get_handle($lang);
			$lh->fail_with(sub { undef });
			$lh;
		}
		or croak 'could not get localization handle';

	my $localized = $localizer->maketext($self->message, $self->args->@*);
	$localized //= $localizers{auto}->maketext($self->message, $self->args->@*)
		unless $self->id;

	croak "did not find translation for ($lang): " . $self->message
		unless defined $localized;

	return $localized;
}

sub translate_lore ($self, $lang)
{
	state $repo = DI->get('lore_data_repo');

	my $translation = $repo->load($self->message)->data->translations->{lc $lang}{$self->args->[0]};

	die "no translation for $lang and " . $self->message
		unless defined $translation;
	return $translation;
}

1;

