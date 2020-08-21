const nodemailer = require("nodemailer");

exports.sendEmail_ = async function({ host, user, password}, to, content) {
      var transporter = await nodemailer.createTransport({
              port: 465,
              auth: {
                    user,
                    pass: password
              },
              host
      });

      await transporter.sendMail({
              from: `"MelanChat" <noreply@melan.chat>`,
              subject: "Reset password",
              html: content,
              to
      });
}

