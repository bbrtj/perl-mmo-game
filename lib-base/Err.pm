package Err;

use v5.42;

# general
use constant ERROR => 'err';

# combat
use constant NOT_IN_LOS => 'err.not_in_los';
use constant CANNOT_MOVE => 'err.cannot_move';
use constant OUT_OF_RANGE => 'err.out_of_range';
use constant INVALID_ACTION => 'err.invalid_action';
use constant ACTION_IN_PROGRESS => 'err.action_in_progress';
use constant INVALID_COORDINATE => 'err.invalid_coordinate';
use constant INVALID_TARGET => 'err.invalid_target';

# chat
use constant PLAYER_NOT_FOUND => 'err.player_not_found';

# registration
use constant EMAIL_TAKEN => 'err.email_taken';
use constant PASSWORD_TOO_SHORT => 'err.password_too_short[]';
use constant PASSWORD_MUST_HAVE_DIGIT => 'err.password_must_have_digit';
use constant PASSWORDS_MISMATCH => 'err.passwords_mismatch';

# player creation
use constant NAME_MUST_CONSIST_OF_LETTERS => 'err.name_must_consist_of_letters';
use constant NAME_TOO_SHORT => 'err.name_too_short[]';
use constant NAME_TOO_LONG => 'err.name_too_long[]';
use constant INVALID_ELEMENT => 'err.invalid_element';

# login
use constant INVALID_CREDENTIALS => 'err.invalid_credentials';
use constant LOGIN_FAILED => 'err.login_failed';

