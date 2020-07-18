const nodemailer = require("nodemailer");

exports.sendEmail_ = async function(to, content) {
      var transporter = await nodemailer.createTransport({
              port: 465,
              auth: {
                    user: process.env.EMAIL_USER,
                    pass: process.env.EMAIL_PASSWORD
              },
              host: process.env.EMAIL_HOST
      });

      await transporter.sendMail({
              from: '"MelanChat" <noreply@melan.chat>',
              subject: "Hello ✔",
              html: "<b>Hello world?</b>",
              to
      });
}

