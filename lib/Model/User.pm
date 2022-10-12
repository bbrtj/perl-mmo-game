package Model::User;

use My::Moose;
use Crypt::PRNG qw(random_bytes);
use Crypt::Bcrypt qw(bcrypt bcrypt_check);

use header;

extends 'Model';

with 'Model::Role::Stored';

use constant BCRYPT_SUBTYPE => '2b';
use constant BCRYPT_COST => '10';

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

sub _make_password ($self, $plaintext_password)
{
	return bcrypt($plaintext_password, BCRYPT_SUBTYPE, BCRYPT_COST, random_bytes(16));
}

sub set_password ($self, $plaintext_password)
{
	$self->_set_password($self->_make_password($plaintext_password));
	return;
}

sub verify_password ($self, $plaintext_password)
{
	return bcrypt_check($plaintext_password, $self->password);
}

__PACKAGE__->_register;

