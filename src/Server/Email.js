import nodemailer from "nodemailer";

export async function sendEmail_({ host, user, password }, to, subject, content) {
      var transporter = await nodemailer.createTransport({
            port: 465,
            auth: {
                  user,
                  pass: password
            },
            host
      });

      await transporter.sendMail({
            from: `"MeroChat" <noreply@mero.chat>`,
            subject,
            html: content,
            to
      });
}
