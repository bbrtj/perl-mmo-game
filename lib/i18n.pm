package i18n;

# very basic structure because it is used in header

use Moo;
use Exporter qw(import);
use Mojo::File qw(curfile);
use Data::Localize;
use Types::Standard qw(Str ArrayRef);
use Carp qw(croak);

# EXPORTED INTERFACE

our @EXPORT = qw(
	_t
	_tt
);

our $CURRENT_LANG = undef;

sub _t
{
	my ($message, @args) = @_;

	return __PACKAGE__->new(
		message => $message,
		args => \@args,
	);
}

sub _tt
{
	my $t = _t @_;
	$t->id(0);

	return $t;
}

# OO INTERFACE

use overload
	q{""} => "translate",
	bool => sub { 1 },
	fallback => 1;

has 'id' => (
	is => 'rw',
	default => sub { 1 },
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
	);
	$loc;
};

sub translate
{
	my ($self) = @_;

	croak 'could not translate (no lang): ' . $self->message
		if !defined $CURRENT_LANG && $self->id;

	$localizer->auto(!$self->id);
	$localizer->set_languages($CURRENT_LANG // (), 'en');

	return $localizer->localize($self->message, $self->args->@*);
}

