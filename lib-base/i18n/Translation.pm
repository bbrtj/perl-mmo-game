package i18n::Translation;

use v5.36;

use My::Moose;
use Mojo::File qw(curfile);
use Data::Localize;
use Types;
use Carp qw(croak);
use Data::Localize::Format::Maketext;
use DI;

use overload
	q{""} => "translate",
	bool => sub { 1 },
	fallback => 1;

has param 'id' => (
	isa => Types::Bool,
	default => sub { 1 },
);

has param 'lore' => (
	isa => Types::Bool,
	writer => 1,
	default => sub { 0 },
);

has param 'message' => (
	isa => Types::Str,
);

has param 'args' => (
	isa => Types::ArrayRef,
	default => sub { [] },
);

my $localizer = do {
	my $loc = Data::Localize->new();

	$loc->add_localizer(
		class => 'Gettext',
		path => curfile->dirname->dirname->sibling('i18n')->child('*.po')->to_string,
		formatter => Data::Localize::Format::Maketext->new,
	);
	$loc;
};

# NOTE: used in overload
sub translate ($self, @)
{
	croak 'could not translate (no lang): ' . $self->message
		if !defined $i18n::CURRENT_LANG && $self->id;

	my $lang = $i18n::CURRENT_LANG // 'en';

	return $self->translate_lore($lang)
		if $self->lore;

	return $self->translate_gettext($lang);

}

sub translate_gettext ($self, $lang)
{
	my $is_id = $self->id;

	$localizer->auto(!$is_id);
	$localizer->set_languages($lang);

	my $localized = $localizer->localize($self->message, $self->args->@*);

	croak "did not find translation for ($lang): " . $self->message
		if !defined $localized && $is_id;

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

