package i18n;

# very basic structure because it is used in header

use v5.32;
use warnings;

use My::Moose;
use Exporter qw(import);
use Mojo::File qw(curfile);
use Data::Localize;
use Types::Standard qw(Str ArrayRef Bool);
use Carp qw(croak);
use Data::Localize::Format::Maketext;

# EXPORTED INTERFACE

our @EXPORT = qw(
	_t
	_tt
	_tph
	_lt
);

our $CURRENT_LANG = undef;

# simple translate
# uses a message id that won't get translated directly
sub _t
{
	my ($message, @args) = @_;

	my ($actual_message, @more_args) = split /\|/, $message;
	@args = @more_args
		if @more_args > 0;

	return __PACKAGE__->new(
		message => $actual_message,
		args => \@args,
	);
}

# text translate
# a text in English that will be returned. Can be used to nest translations
sub _tt
{
	my ($message, @args) = @_;

	return __PACKAGE__->new(
		message => $message,
		args => \@args,
		id => 0,
	);
}

# placeholder translate
# will add values to placeholders in the string. Does not actually translate
# used to transfer translation strings and placeholders to client where they are translated
sub _tph
{
	return join '|', @_;
}

# lore translate
# will translate using database lore data, searching for the id
# the second parameters should be a type - name or description
sub _lt
{
	my $t = _t shift, shift;
	$t->lore(1);

	return $t;
}

# OO INTERFACE

use overload
	q{""} => "translate",
	bool => sub { 1 },
	fallback => 1;

has 'id' => (
	is => 'rw',
	isa => Bool,
	default => sub { 1 },
);

has 'lore' => (
	is => 'rw',
	isa => Bool,
	default => sub { 0 },
);

has 'message' => (
	is => 'ro',
	isa => Str,
	required => 1,
);

has 'args' => (
	is => 'ro',
	isa => ArrayRef,
	default => sub { [] },
);

my $localizer = do {
	my $loc = Data::Localize->new();

	$loc->add_localizer(
		class => 'Gettext',
		path => curfile->dirname->sibling('i18n')->child('*.po')->to_string,
		formatter => Data::Localize::Format::Maketext->new,
	);
	$loc;
};

sub translate
{
	my ($self) = @_;

	croak 'could not translate (no lang): ' . $self->message
		if !defined $CURRENT_LANG && $self->id;

	my $lang = $CURRENT_LANG // 'en';

	return $self->translate_lore($lang)
		if $self->lore;

	return $self->translate_gettext($lang);

}

sub translate_gettext
{
	my ($self, $lang) = @_;
	my $is_id = $self->id;

	$localizer->auto(!$is_id);
	$localizer->set_languages($lang);

	my $localized = $localizer->localize($self->message, $self->args->@*);

	croak 'did not find translation for: ' . $self->message
		if !defined $localized && $is_id;

	return $localized;
}

sub translate_lore
{
	my ($self, $lang) = @_;

	require DI;    # lazy load to avoid circularity
	state $repo = DI->get('lore_data');

	my $translation = $repo->load($self->message)->data->translations->{lc $lang}{$self->args->[0]};

	die "no translation for $lang and " . $self->message
		unless defined $translation;
	return $translation;
}

1;

