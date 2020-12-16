package Game::Model::User;

use Mojo::Base -signatures;
use Moose;
use Game::Types qw(EmailAddress NonEmptySimpleStr PositiveInt DateTime);
use Digest::SHA qw(sha256_hex);

with 'Game::Model';

has 'email' => (
	is => 'rw',
	isa => EmailAddress,
	required => 1,
);

has 'password' => (
	is => 'ro',
	isa => NonEmptySimpleStr->where(q{ length $_ == 64 }),
	writer => '_set_password',
	required => 1,
);

has 'salt' => (
	is => 'ro',
	isa => NonEmptySimpleStr->where(q{ length $_ == 16 }),
	lazy => 1,
	default => sub ($self) {
		substr sha256_hex($self->uuid . time), 0, 16;
	},
);

has 'status' => (
	is => 'rw',
	isa => PositiveInt,
	default => sub { 1 },
);

has 'created_at' => (
	is => 'ro',
	isa => DateTime,
	default => sub { time },
);

sub _make_password($self, $plaintext_password)
{
	return sha256_hex(sha256($plaintext_password) . $self->salt);
}

sub store_password($self, $plaintext_password)
{
	$self->_set_password($self->_make_password($plaintext_password));
	return;
}

sub verify_password($self, $plaintext_password)
{
	return $self->password eq $self->_make_password($plaintext_password);
}

__PACKAGE__->_register;
