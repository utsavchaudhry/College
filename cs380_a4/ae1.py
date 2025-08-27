import sys
import torch.nn as nn
from autoencoder import _Autoencoder
from data import Data
from model import Model

class AE1(_Autoencoder):
    def __init__(self, path):
        super().__init__(path)

        n_kernels = 64
        
        self.encoder = Model(
            input_shape=(self.BATCH_SIZE, 3, 64, 64),
            layers=[
                nn.Conv2d(3, n_kernels, kernel_size=3, padding=1),
                nn.ReLU(),
                nn.MaxPool2d(kernel_size=2)
            ]
        )

        self.decoder = Model(
            input_shape=(self.BATCH_SIZE, n_kernels, 32, 32),
            layers=[
                nn.ConvTranspose2d(n_kernels, 3, kernel_size=3, stride=2, padding=1, output_padding=1),
                nn.Sigmoid()
            ]
        )

        self.model = Model(
            input_shape=self.encoder.input_shape,
            layers=[self.encoder, self.decoder]
        )

if __name__ == '__main__':
    
    epochs = int(sys.argv[1]) if len(sys.argv) > 1 else None
    data = Data.load('data', image_size=64)
    data.shuffle()
    ae1 = AE1('models/ae1.pt')
    ae1.print()

    if not epochs:
        print(f'\nLoading {ae1.path}...')
        ae1.load()
    else:
        print(f'\nTraining AE1 for {epochs} epochs...')
        ae1.train(epochs, data)
        print(f'\nSaving model to {ae1.path}...')
        ae1.save()
    
    print(f'\nGenerating reconstructed samples with AE1...')
    reconstructed = ae1.generate(data)
    
    data.display(16)
    reconstructed.display(16)
