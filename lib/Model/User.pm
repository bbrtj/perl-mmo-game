package Model::User;

use My::Moose;
use Types;
use Digest::SHA qw(sha256);
use Crypt::Eksblowfish::Bcrypt qw(bcrypt en_base64);

use header;

extends 'Model';

with 'Model::Role::Stored';

use constant BCRYPT_SETTINGS => '$2a$10$';

has 'email' => (
	is => 'ro',
	isa => Types::EmailAddress,
	trigger => sub ($self, $value, @) {
		$self->{email} = lc $value;
	},
	required => 1,
);

has 'password' => (
	is => 'ro',
	isa => Types::NonEmptySimpleStr->where(q{ length $_ <= 60 }),
	writer => '_set_password',
	required => 1,
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

sub _gen_salt ($self)
{
	return en_base64(substr sha256($self->id . rand), 0, 16);
}

sub _make_password ($self, $plaintext_password, $settings = BCRYPT_SETTINGS . $self->_gen_salt)
{
	return bcrypt($plaintext_password, $settings);
}

sub set_password ($self, $plaintext_password)
{
	$self->_set_password($self->_make_password($plaintext_password));
	return;
}

sub verify_password ($self, $plaintext_password)
{
	return $self->password eq $self->_make_password($plaintext_password, $self->password);
}

__PACKAGE__->_register;
