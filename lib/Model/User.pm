package Model::User;

use My::Moose;
use Crypt::PRNG qw(random_bytes);
use Crypt::Bcrypt qw(bcrypt bcrypt_check);
use Digest::MD5 qw(md5_hex);

use header;

extends 'Model';

with 'Model::Role::Stored';

use constant BCRYPT_SUBTYPE => '2b';
use constant BCRYPT_COST => '9';

has param 'email' => (
	isa => EmailAddress,
	trigger => sub ($self, $value, @) {
		my $original = $value;
		$value = lc $value;
		if ($original ne $value) {
			$self->set_email($value);
		}
	},
);

has param 'password' => (
	isa => NonEmptySimpleStr->where(q{ length $_ <= 60 }),
	writer => -hidden,
);

has param 'status' => (
	isa => PositiveInt,
	default => 1,
);

has param 'created_at' => (
	coerce => DateTime,
	default => sub { time },
);

around BUILDARGS => sub ($orig, $self, %args) {
	foreach my $type (qw(plaintext_password hashed_password)) {
		$args{password} = $self->_make_password($type, delete $args{$type})
			if $args{$type};
	}

	return $self->$orig(%args);
};

sub _bcrypt_password ($self, $hashed_password)
{
	return bcrypt($hashed_password, BCRYPT_SUBTYPE, BCRYPT_COST, random_bytes(16));
}

sub _hash_password ($self, $plaintext_password)
{
	return md5_hex($plaintext_password);
}

sub _make_password ($self, $type, $password)
{
	if ($type eq 'plaintext_password') {
		$password = $self->_hash_password($password);
		$type = 'hashed_password';
	}

	if ($type eq 'hashed_password') {
		$password = $self->_bcrypt_password($password);
		$type = 'password';
	}

	return $password;
}

sub set_hashed_password ($self, $password)
{
	$self->_set_password($self->_make_password(hashed_password => $password));
	return;
}

sub set_plaintext_password ($self, $password)
{
	$self->_set_password($self->_make_password(plaintext_password => $password));
	return;
}

sub verify_password ($self, $hashed_password)
{
	return bcrypt_check($hashed_password, $self->password);
}

__PACKAGE__->_register;

