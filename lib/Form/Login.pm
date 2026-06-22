package Form::Login;

use My::Moose;
use My::Form::Tiny;
use Digest::MD5 qw(md5_hex);

use header;

use constant needs_hashing => 0;

has field 'user' => (
	writer => 1,
	isa => InstanceOf ['Model::User'],
);

form_field 'email' => (
	type => SimpleStr,
	required => 1,
	data => {t => 'email', p => _t('email_address'), l => undef},
);

form_field 'password' => (
	type => SimpleStr,
	required => 1,
	data => {t => 'password', p => _t('password'), l => undef},
	adjust => sub ($self, $value) {
		$value = md5_hex($value) if $self->needs_hashing;
		return $value;
	},
);

form_cleaner sub ($self, $data) {
	try {
		my $user = DI->get('user_service')->find_user_by_email($data->{email});
		if (!$user->verify_password($data->{password})) {
			$self->add_error(Err::INVALID_CREDENTIALS);
		}
		else {
			$self->set_user($user);
		}
	}
	catch ($e) {
		die $e
			unless $e isa X::RecordDoesNotExist;

		$self->add_error(Err::INVALID_CREDENTIALS);
	}
};

