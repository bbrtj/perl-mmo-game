package Game::Model::User;

use Moose;
use Types;
use Digest::SHA qw(sha256_hex);

use header;

with 'Game::Model', 'Game::Model::Role::Stored';

has 'email' => (
	is => 'ro',
	isa => Types::EmailAddress,
	required => 1,
);

has 'password' => (
	is => 'ro',
	isa => Types::NonEmptySimpleStr->where(q{ length $_ == 64 }),
	writer => '_set_password',
	required => 1,
);

has 'salt' => (
	is => 'ro',
	isa => Types::NonEmptySimpleStr->where(q{ length $_ == 16 }),
	lazy => 1,
	default => sub ($self) {
		substr sha256_hex($self->id . time), 0, 16;
	},
);

has 'status' => (
	is => 'ro',
	isa => Types::PositiveInt,
	default => sub { 1 },
);

has 'created_at' => (
	is => 'ro',
	isa => Types::DateTime,
	coerce => 1,
	default => sub { time },
);

# TODO: bcrypt
sub _make_password ($self, $plaintext_password)
{
	return sha256_hex(sha256_hex($plaintext_password) . $self->salt);
}

sub set_password ($self, $plaintext_password)
{
	$self->_set_password($self->_make_password($plaintext_password));
	return;
}

sub verify_password ($self, $plaintext_password)
{
	return $self->password eq $self->_make_password($plaintext_password);
}

__PACKAGE__->_register;
