package i18n;

# very basic structure because it is used in header

use v5.32;
use Moo;
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
	_lt
);

our $CURRENT_LANG = undef;

# simple translate
# uses a message id that won't get translated directly
sub _t
{
	my ($message, @args) = @_;

	return __PACKAGE__->new(
		message => $message,
		args => \@args,
	);
}

# text translate
# a text in English that will get translated if translation is not found
sub _tt
{
	my $t = _t lc shift, @_;
	$t->id(0);

	return $t;
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
		path => curfile->dirname->to_string . '/i18n/*.po',
		formatter => Data::Localize::Format::Maketext->new,
	);
	$loc;
};

sub translate
{
	my ($self) = @_;

	croak 'could not translate (no lang): ' . $self->message
		if !defined $CURRENT_LANG && $self->id;

	if ($self->lore) {
		return $self->translate_lore;
	}

	$localizer->auto(!$self->id);
	$localizer->set_languages($CURRENT_LANG // (), 'en');

	my $localized = $localizer->localize($self->message, $self->args->@*);
	$localized = ucfirst $localized
		unless $self->id;

	return $localized;
}

sub translate_lore
{
	my ($self) = @_;

	require DI;    # lazy load to avoid circularity
	state $repo = DI->get('lore_data');

	return $repo->load($self->args->[0], $self->message, $CURRENT_LANG);
}

1;
