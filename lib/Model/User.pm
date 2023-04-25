package Model::User;

use My::Moose;
use Crypt::PRNG qw(random_bytes);
use Crypt::Bcrypt qw(bcrypt bcrypt_check);
use Digest::MD5 qw(md5_hex);

use header;

extends 'Model';

with 'Model::Role::Stored';

# TODO: SHA256 passwords before bcrypt?

use constant BCRYPT_SUBTYPE => '2b';
use constant BCRYPT_COST => '10';

has param 'email' => (
	isa => Types::EmailAddress,
	trigger => sub ($self, $value, @) {
		my $original = $value;
		$value = lc $value;
		if ($original ne $value) {
			$self->set_email($value);
		}
	},
);

has param 'password' => (
	isa => Types::NonEmptySimpleStr->where(q{ length $_ <= 60 }),
	writer => -hidden,
);

has param 'status' => (
	isa => Types::PositiveInt,
	default => 1,
);

has param 'created_at' => (
	coerce => Types::DateTime,
	default => sub { time },
);

around BUILDARGS => sub ($orig, $self, %args) {
	if ($args{plaintext_password}) {
		$args{password} = $self->_make_password(delete $args{plaintext_password});
	}

	return $self->$orig(%args);
};

sub _make_password ($self, $plaintext_password)
{
	return bcrypt(md5_hex($plaintext_password), BCRYPT_SUBTYPE, BCRYPT_COST, random_bytes(16));
}

sub set_password ($self, $plaintext_password)
{
	$self->_set_password($self->_make_password($plaintext_password));
	return;
}

sub verify_password ($self, $hashed_password)
{
	return bcrypt_check($hashed_password, $self->password);
}

__PACKAGE__->_register;

